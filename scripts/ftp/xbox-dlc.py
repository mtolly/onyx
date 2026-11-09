#!/usr/bin/env python3
"""Manage Xbox 360 DLC (STFS CON/LIVE packages) over FTP.

Usage:
  xbox-dlc.py list [TITLEID...]       list DLC, grouped by game
  xbox-dlc.py get REMOTE... [-o DIR]  download files (paths relative to the root)
  xbox-dlc.py put LOCAL... [-f]       upload files to the folder their header says
  xbox-dlc.py rm REMOTE... [-y]       delete files
  xbox-dlc.py info LOCAL...           show metadata of local STFS files
"""

# ---- Configuration ----------------------------------------------------------
XBOX_IP = "192.168.1.242"
FTP_USER = "xboxftp"
FTP_PASS = "xboxftp"
CONTENT_ROOT = "/Hdd1/Content/0000000000000000"
# -----------------------------------------------------------------------------

import argparse
import fnmatch
import ftplib
import os
import posixpath
import re
import sys
import time
from collections import Counter

# STFS layout: 4-byte magic, 0x228-byte header, then metadata starting at 0x22C.
HEADER_READ_SIZE = 0x1711
OFF_TITLE_ID = 0x360        # u32 big-endian
OFF_DISPLAY_NAME = 0x411    # 9 locales x 0x100 bytes, UTF-16BE
OFF_TITLE_NAME = 0x1691     # 0x80 bytes, UTF-16BE

FOLDER_FOR_MAGIC = {b"CON ": "00000001", b"LIVE": "00000002"}
KIND_FOR_FOLDER = {v: k.decode().strip() for k, v in FOLDER_FOR_MAGIC.items()}
TITLE_ID_RE = re.compile(r"^[0-9A-Fa-f]{8}$")


def utf16_field(data, offset, size):
    return data[offset:offset + size].decode("utf-16-be", errors="replace").split("\0")[0].strip()


class StfsInfo:
    def __init__(self, data):
        if len(data) < HEADER_READ_SIZE:
            raise ValueError("file too short to be STFS")
        self.magic = data[:4]
        if self.magic not in FOLDER_FOR_MAGIC:
            raise ValueError(f"not a CON/LIVE file (magic {self.magic!r})")
        self.kind = self.magic.decode().strip()
        self.folder = FOLDER_FOR_MAGIC[self.magic]
        self.title_id = f"{int.from_bytes(data[OFF_TITLE_ID:OFF_TITLE_ID + 4], 'big'):08X}"
        # first non-empty locale (English is first)
        self.display_name = ""
        for i in range(9):
            name = utf16_field(data, OFF_DISPLAY_NAME + i * 0x100, 0x100)
            if name:
                self.display_name = name
                break
        self.title_name = utf16_field(data, OFF_TITLE_NAME, 0x80)

    @property
    def remote_dir(self):
        return f"{self.title_id}/{self.folder}"


def fmt_size(n):
    for unit in ("B", "KB", "MB", "GB"):
        if n < 1024 or unit == "GB":
            return f"{n:.0f} {unit}" if unit == "B" else f"{n:.1f} {unit}"
        n /= 1024


class Progress:
    def __init__(self, label, total):
        self.label, self.total, self.done = label, total, 0
        self.start = self.last = time.time()
        self.tty = sys.stderr.isatty()

    def update(self, n):
        self.done += n
        now = time.time()
        if self.tty and now - self.last >= 0.2:
            self.last = now
            self._print()

    def _print(self, end=""):
        elapsed = max(time.time() - self.start, 1e-6)
        pct = f"{100 * self.done / self.total:5.1f}%" if self.total else ""
        speed = fmt_size(self.done / elapsed) + "/s"
        msg = f"\r{self.label}: {fmt_size(self.done)} / {fmt_size(self.total or 0)} {pct} {speed}"
        sys.stderr.write(msg.ljust(100) + end)
        sys.stderr.flush()

    def finish(self):
        self._print(end="\n")


class Xbox:
    def __init__(self):
        self.ftp = ftplib.FTP(XBOX_IP, timeout=30, encoding="utf-8")
        self.ftp.login(FTP_USER, FTP_PASS)
        self.ftp.voidcmd("TYPE I")

    def close(self):
        try:
            self.ftp.quit()
        except Exception:
            self.ftp.close()

    def abs_path(self, path):
        path = path.replace("\\", "/")
        return path if path.startswith("/") else posixpath.join(CONTENT_ROOT, path)

    def listdir(self, path):
        """Returns [(name, is_dir, size)]. This server ignores LIST arguments, so cwd first."""
        self.ftp.cwd(self.abs_path(path))
        lines = []
        self.ftp.retrlines("LIST", lines.append)
        entries = []
        for line in lines:
            parts = line.split(None, 8)
            if len(parts) < 9:
                continue
            entries.append((parts[8], parts[0].startswith("d"), int(parts[4])))
        return entries

    def expand(self, pattern):
        """Expands a remote path with glob wildcards (* ? [..]) in any component.
        Returns [(path, is_dir, size)], matching names case-insensitively."""
        absolute = pattern.startswith("/")
        parts = [p for p in pattern.replace("\\", "/").split("/") if p]
        results = [("/" if absolute else "", True, 0)]
        for part in parts:
            matched = []
            for path, is_dir, _ in results:
                if not is_dir:
                    continue
                try:
                    entries = self.listdir(path)
                except ftplib.error_perm:
                    continue
                for name, child_dir, size in entries:
                    if fnmatch.fnmatchcase(name.lower(), part.lower()):
                        matched.append((posixpath.join(path, name), child_dir, size))
            results = matched
        return sorted(results)

    def exists_dir(self, path):
        try:
            self.ftp.cwd(self.abs_path(path))
            return True
        except ftplib.error_perm:
            return False

    def read_head(self, path, size=HEADER_READ_SIZE):
        """Download only the first `size` bytes of a file."""
        conn = self.ftp.transfercmd("RETR " + self.abs_path(path))
        buf = b""
        try:
            while len(buf) < size:
                chunk = conn.recv(size - len(buf))
                if not chunk:
                    break
                buf += chunk
        finally:
            conn.close()
        # Aborting early gives "550 Connection interrupted"; a full read gives 226.
        try:
            self.ftp.getresp()
        except ftplib.error_perm:
            pass
        return buf

    def download(self, path, local_path, size=None):
        prog = Progress(posixpath.basename(path), size)
        tmp = local_path + ".part"
        with open(tmp, "wb") as fd:
            def write(chunk):
                fd.write(chunk)
                prog.update(len(chunk))
            self.ftp.retrbinary("RETR " + self.abs_path(path), write, blocksize=1 << 16)
        prog.finish()
        os.replace(tmp, local_path)

    def upload(self, local_path, remote_path):
        size = os.path.getsize(local_path)
        prog = Progress(os.path.basename(local_path), size)
        with open(local_path, "rb") as fd:
            self.ftp.storbinary("STOR " + self.abs_path(remote_path), fd,
                                blocksize=1 << 16, callback=lambda c: prog.update(len(c)))
        prog.finish()

    def makedirs(self, path):
        cur = CONTENT_ROOT
        for part in path.split("/"):
            cur = posixpath.join(cur, part)
            if not self.exists_dir(cur):
                self.ftp.mkd(cur)


# ---- Commands ----------------------------------------------------------------

def cmd_list(xbox, args):
    wanted = {t.upper() for t in args.title_ids}
    titles = sorted(name.upper() for name, is_dir, _ in xbox.listdir("")
                    if is_dir and TITLE_ID_RE.match(name))
    if wanted:
        titles = [t for t in titles if t in wanted]
    total_files = 0
    for title in titles:
        subdirs = {name for name, is_dir, _ in xbox.listdir(title) if is_dir}
        files = []  # (relpath, kind, size, StfsInfo or error string)
        for folder, kind in KIND_FOR_FOLDER.items():
            if folder not in subdirs:
                continue
            for name, is_dir, size in xbox.listdir(f"{title}/{folder}"):
                if is_dir:
                    continue
                rel = f"{title}/{folder}/{name}"
                try:
                    info = StfsInfo(xbox.read_head(rel))
                except (ValueError, ftplib.Error) as e:
                    info = str(e)
                files.append((rel, kind, size, info))
        if not files:
            continue
        # Files can carry the wrong title name (e.g. RB2 customs under RB3's ID), so take a vote.
        names = Counter(i.title_name for *_, i in files if isinstance(i, StfsInfo) and i.title_name)
        title_name = names.most_common(1)[0][0] if names else "(unknown title)"
        print(f"{title}  {title_name}")
        for rel, kind, size, info in sorted(files, key=lambda f: (
                f[3].display_name.lower() if isinstance(f[3], StfsInfo) else "", f[0])):
            label = info.display_name if isinstance(info, StfsInfo) else f"[{info}]"
            print(f"  {kind:<4} {fmt_size(size):>9}  {label}")
            if args.paths:
                print(f"                  {rel}")
        print()
        total_files += len(files)
    print(f"{total_files} file(s)")


def cmd_get(xbox, args):
    os.makedirs(args.output, exist_ok=True)
    for remote, is_dir, size in expand_all(xbox, args.remote):
        if is_dir:
            print(f"Skipping {remote}: is a folder (use {remote}/* for its files)")
            continue
        name = posixpath.basename(remote)
        local = os.path.join(args.output, name)
        if os.path.exists(local) and not args.force:
            print(f"Skipping {name}: {local} already exists (use -f to overwrite)")
            continue
        xbox.download(remote, local, size)


def cmd_put(xbox, args):
    for local in args.local:
        with open(local, "rb") as fd:
            try:
                info = StfsInfo(fd.read(HEADER_READ_SIZE))
            except ValueError as e:
                print(f"Skipping {local}: {e}")
                continue
        name = os.path.basename(local)
        remote_dir = info.remote_dir
        remote = f"{remote_dir}/{name}"
        print(f"{name} -> {remote}  ({info.title_name} / {info.display_name})")
        if xbox.exists_dir(remote_dir):
            existing = {n for n, _, _ in xbox.listdir(remote_dir)}
            if name in existing and not args.force:
                print(f"  already exists on console, skipping (use -f to overwrite)")
                continue
        else:
            xbox.makedirs(remote_dir)
        xbox.upload(local, remote)


def expand_all(xbox, patterns):
    """Expands each pattern, exiting if one matches nothing. Removes duplicates."""
    seen, out = set(), []
    for pattern in patterns:
        matches = xbox.expand(pattern)
        if not matches:
            sys.exit(f"No match on console: {pattern}")
        for m in matches:
            if m[0] not in seen:
                seen.add(m[0])
                out.append(m)
    return out


def cmd_rm(xbox, args):
    targets = []
    for remote, is_dir, size in expand_all(xbox, args.remote):
        if is_dir:
            print(f"Skipping {remote}: is a folder")
            continue
        label = remote
        try:
            info = StfsInfo(xbox.read_head(remote))
            label = f"{remote}  ({info.title_name} / {info.display_name})"
        except (ValueError, ftplib.Error):
            pass
        targets.append((remote, label, size))
    if not targets:
        return
    print("Will delete:")
    for _, label, size in targets:
        print(f"  {fmt_size(size):>9}  {label}")
    if not args.yes:
        if input(f"Delete {len(targets)} file(s)? [y/N] ").strip().lower() != "y":
            return
    for remote, _, _ in targets:
        xbox.ftp.delete(xbox.abs_path(remote))
        print(f"Deleted {remote}")


def cmd_info(args):
    for local in args.local:
        with open(local, "rb") as fd:
            try:
                info = StfsInfo(fd.read(HEADER_READ_SIZE))
            except ValueError as e:
                print(f"{local}: {e}")
                continue
        print(f"{local}\n  {info.kind}  {info.title_id}  {info.title_name}\n"
              f"  {info.display_name}\n  -> {info.remote_dir}/")


def main():
    p = argparse.ArgumentParser(description="Manage Xbox 360 DLC over FTP.")
    sub = p.add_subparsers(dest="cmd", required=True)

    s = sub.add_parser("list", aliases=["ls"], help="list DLC grouped by game")
    s.add_argument("title_ids", nargs="*", help="only these title IDs")
    s.add_argument("-p", "--paths", action="store_true", help="show remote paths")

    s = sub.add_parser("get", help="download files")
    s.add_argument("remote", nargs="+", help="e.g. 45410914/00000001/somefile")
    s.add_argument("-o", "--output", default=".", help="local folder (default: current)")
    s.add_argument("-f", "--force", action="store_true", help="overwrite local files")

    s = sub.add_parser("put", help="upload STFS files to their correct folders")
    s.add_argument("local", nargs="+")
    s.add_argument("-f", "--force", action="store_true", help="overwrite files on console")

    s = sub.add_parser("rm", help="delete files from the console")
    s.add_argument("remote", nargs="+")
    s.add_argument("-y", "--yes", action="store_true", help="don't ask for confirmation")

    s = sub.add_parser("info", help="show metadata of local STFS files")
    s.add_argument("local", nargs="+")

    args = p.parse_args()
    if args.cmd == "info":
        return cmd_info(args)
    xbox = Xbox()
    try:
        {"list": cmd_list, "ls": cmd_list, "get": cmd_get,
         "put": cmd_put, "rm": cmd_rm}[args.cmd](xbox, args)
    finally:
        xbox.close()


if __name__ == "__main__":
    try:
        main()
    except KeyboardInterrupt:
        sys.exit("\nInterrupted")
