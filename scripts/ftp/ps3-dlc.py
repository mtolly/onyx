#!/usr/bin/env python3
"""Manage PlayStation 3 DLC over FTP.

.pkg files are not copied to the console as-is: their contents are extracted
(streamed and decrypted on the fly) into the folder for their game ID, e.g.
/dev_hdd0/game/BLUS30463/USRDIR/... Files inside the pkg such as .edat are
left exactly as stored (the console decrypts those itself).

Usage:
  ps3-dlc.py list [GAMEID...]          list games; with IDs, list their files
  ps3-dlc.py get REMOTE... [-o DIR]    download files (paths relative to the root)
  ps3-dlc.py put LOCAL.pkg... [-f]     extract pkg contents to the console
  ps3-dlc.py rm REMOTE... [-r] [-y]    delete files (-r for folders)
  ps3-dlc.py info LOCAL.pkg...         show metadata and contents of local pkgs
  ps3-dlc.py extract LOCAL.pkg [-o DIR]  extract a pkg locally
"""

# ---- Configuration ----------------------------------------------------------
PS3_IP = "192.168.1.247"
FTP_USER = "anonymous"
FTP_PASS = ""
CONTENT_ROOT = "/dev_hdd0/game"
# -----------------------------------------------------------------------------

import argparse
import fnmatch
import ftplib
import hashlib
import os
import posixpath
import re
import struct
import sys
import time

from cryptography.hazmat.primitives.ciphers import Cipher, algorithms, modes

GAME_ID_RE = re.compile(r"^[A-Z]{4}\d{5}$")

# ---- PKG format (see PKG.hs / https://www.psdevwiki.com/ps3/PKG_files) ------

PKG_MAGIC = b"\x7fPKG"
PS3_GPKG_AES_KEY = bytes.fromhex("2E7B71D7C9C9A14EA3221F188828B8F8")

FLAG_OVERWRITE = 0x80000000  # absent on game metadata (PARAM.SFO etc.): don't replace
TYPE_FOLDER = 0x04


class PkgItem:
    def __init__(self, name, offset, size, flags):
        self.name, self.offset, self.size, self.flags = name, offset, size, flags

    @property
    def is_dir(self):
        return (self.flags & 0xFF) == TYPE_FOLDER

    @property
    def overwrite(self):
        return bool(self.flags & FLAG_OVERWRITE)


class Pkg:
    def __init__(self, path):
        self.path = path
        self.fd = open(path, "rb")
        h = self.fd.read(0x80)
        if len(h) < 0x80 or h[:4] != PKG_MAGIC:
            raise ValueError("not a PS3 .pkg file")
        revision, pkg_type = struct.unpack(">HH", h[4:8])
        if pkg_type != 1:
            raise ValueError(f"not a PS3 pkg (type {pkg_type})")
        if revision not in (0x8000, 0x0000):
            raise ValueError(f"unknown pkg revision {revision:#06x}")
        self.finalized = revision == 0x8000
        self.item_count, = struct.unpack(">I", h[0x14:0x18])
        self.data_offset, self.data_size = struct.unpack(">QQ", h[0x20:0x30])
        self.content_id = h[0x30:0x60].split(b"\0")[0].decode("ascii", "replace")
        qa_digest = h[0x60:0x70]
        riv = h[0x70:0x80]
        # content ID looks like UP8802-BLUS30463_00-SOMETHING; game ID is at 0x37
        self.game_id = h[0x37:0x40].decode("ascii", "replace")
        if not GAME_ID_RE.match(self.game_id):
            raise ValueError(f"unexpected game ID {self.game_id!r} in content ID {self.content_id!r}")

        if self.finalized:
            self._riv = int.from_bytes(riv, "big")
        else:
            k = qa_digest
            self._prefix = k[:8] + k[:8] + k[8:] + k[8:] + bytes(0x18)

        recs = self._read(0, 0x20 * self.item_count)
        self.items = []
        for i in range(self.item_count):
            name_off, name_size, data_off, data_size, flags, _ = \
                struct.unpack(">IIQQII", recs[i * 0x20:(i + 1) * 0x20])
            name = self._read(name_off, name_size).decode("utf-8", "replace").lstrip("/")
            self.items.append(PkgItem(name, data_off, data_size, flags))

    def close(self):
        self.fd.close()

    def __enter__(self):
        return self

    def __exit__(self, *exc):
        self.close()

    def _keystream(self, block, count):
        if self.finalized:
            ctr = ((self._riv + block) % (1 << 128)).to_bytes(16, "big")
            enc = Cipher(algorithms.AES(PS3_GPKG_AES_KEY), modes.CTR(ctr)).encryptor()
            return enc.update(bytes(count * 16))
        sha1, prefix = hashlib.sha1, self._prefix
        return b"".join([sha1(prefix + ((block + i) % (1 << 64)).to_bytes(8, "big")).digest()[:16]
                         for i in range(count)])

    def _read(self, pos, size):
        """Reads and decrypts `size` bytes at `pos` within the data section."""
        if size == 0:
            return b""
        block, skip = divmod(pos, 16)
        nblocks = (skip + size + 15) // 16
        self.fd.seek(self.data_offset + block * 16)
        enc = self.fd.read(nblocks * 16)
        ks = self._keystream(block, nblocks)
        n = len(enc)
        dec = (int.from_bytes(enc, "big") ^ int.from_bytes(ks[:n], "big")).to_bytes(n, "big")
        return dec[skip:skip + size]

    def open_item(self, item):
        return PkgItemReader(self, item)

    @property
    def files(self):
        return [i for i in self.items if not i.is_dir]


class PkgItemReader:
    """File-like object that decrypts one item's contents as it is read."""
    def __init__(self, pkg, item):
        self.pkg, self.item, self.pos = pkg, item, 0

    def read(self, n=-1):
        left = self.item.size - self.pos
        if n is None or n < 0 or n > left:
            n = left
        data = self.pkg._read(self.item.offset + self.pos, n)
        self.pos += len(data)
        return data


# ---- PARAM.SFO --------------------------------------------------------------

def parse_sfo(data):
    """Returns {key: value} from a PARAM.SFO."""
    if data[:4] != b"\0PSF":
        return {}
    key_table, data_table, count = struct.unpack("<III", data[8:20])
    out = {}
    for i in range(count):
        key_off, fmt, length, _, data_off = struct.unpack("<HHIII", data[20 + i * 16:36 + i * 16])
        key = data[key_table + key_off:].split(b"\0")[0].decode("ascii", "replace")
        raw = data[data_table + data_off:data_table + data_off + length]
        if fmt == 0x0404:
            out[key] = int.from_bytes(raw[:4], "little")
        else:
            out[key] = raw.split(b"\0")[0].decode("utf-8", "replace")
    return out


# ---- Helpers ----------------------------------------------------------------

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
        label = self.label if len(self.label) <= 50 else "..." + self.label[-47:]
        msg = f"\r{label}: {fmt_size(self.done)} / {fmt_size(self.total or 0)} {pct} {speed}"
        sys.stderr.write(msg.ljust(110) + end)
        sys.stderr.flush()

    def finish(self):
        if self.tty:
            self._print(end="\n")


class PS3:
    def __init__(self):
        # webMAN's replies contain non-UTF-8 bytes (e.g. "65 °C" in Latin-1), so talk
        # Latin-1 (never fails to decode) and convert file names to/from UTF-8 ourselves.
        self.ftp = ftplib.FTP(PS3_IP, timeout=30, encoding="latin-1")
        self.ftp.login(FTP_USER, FTP_PASS)
        self.ftp.voidcmd("TYPE I")
        self._dir_cache = set()

    def close(self):
        try:
            self.ftp.quit()
        except Exception:
            self.ftp.close()

    def abs_path(self, path):
        path = path.replace("\\", "/")
        return path if path.startswith("/") else posixpath.join(CONTENT_ROOT, path)

    def wire(self, path):
        """Absolute path, encoded for sending over the Latin-1 connection."""
        return self.abs_path(path).encode("utf-8").decode("latin-1")

    def listdir(self, path):
        """Returns [(name, is_dir, size)]. cwd first, since some servers ignore LIST arguments."""
        self.ftp.cwd(self.wire(path))
        lines = []
        self.ftp.retrlines("LIST", lines.append)
        entries = []
        for line in lines:
            parts = line.split(None, 8)
            if len(parts) < 9 or parts[8] in (".", ".."):
                continue
            name = parts[8].encode("latin-1").decode("utf-8", "replace")
            entries.append((name, parts[0].startswith("d"), int(parts[4])))
        return entries

    def walk(self, path):
        """Yields (path, size) for every file under a folder, recursively."""
        for name, is_dir, size in sorted(self.listdir(path)):
            child = posixpath.join(path, name)
            if is_dir:
                yield from self.walk(child)
            else:
                yield child, size

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
            self.ftp.cwd(self.wire(path))
            return True
        except ftplib.error_perm:
            return False

    def read_file(self, path):
        chunks = []
        self.ftp.retrbinary("RETR " + self.wire(path), chunks.append)
        return b"".join(chunks)

    def download(self, path, local_path, size=None):
        prog = Progress(path, size)
        tmp = local_path + ".part"
        with open(tmp, "wb") as fd:
            def write(chunk):
                fd.write(chunk)
                prog.update(len(chunk))
            self.ftp.retrbinary("RETR " + self.wire(path), write, blocksize=1 << 16)
        prog.finish()
        os.replace(tmp, local_path)

    def upload(self, fd, remote_path, size):
        prog = Progress(remote_path, size)
        self.ftp.storbinary("STOR " + self.wire(remote_path), fd,
                            blocksize=1 << 16, callback=lambda c: prog.update(len(c)))
        prog.finish()

    def makedirs(self, path):
        cur = CONTENT_ROOT
        for part in path.split("/"):
            cur = posixpath.join(cur, part)
            if cur in self._dir_cache:
                continue
            if not self.exists_dir(cur):
                self.ftp.mkd(self.wire(cur))
            self._dir_cache.add(cur)

    def game_title(self, game_id):
        try:
            return parse_sfo(self.read_file(f"{game_id}/PARAM.SFO")).get("TITLE", "")
        except ftplib.Error:
            return ""


def expand_all(ps3, patterns):
    """Expands each pattern, exiting if one matches nothing. Removes duplicates."""
    seen, out = set(), []
    for pattern in patterns:
        matches = ps3.expand(pattern)
        if not matches:
            sys.exit(f"No match on console: {pattern}")
        for m in matches:
            if m[0] not in seen:
                seen.add(m[0])
                out.append(m)
    return out


def open_pkg(local):
    try:
        return Pkg(local)
    except (ValueError, OSError) as e:
        print(f"Skipping {local}: {e}")
        return None


# ---- Commands ----------------------------------------------------------------

def cmd_list(ps3, args):
    games = sorted(name for name, is_dir, _ in ps3.listdir("") if is_dir)
    if not args.game_ids:
        for game in games:
            print(f"{game:<10} {ps3.game_title(game)}")
        print(f"\n{len(games)} folder(s). Use 'list GAMEID' to see files.")
        return
    selected = [g for g in games
                if any(fnmatch.fnmatchcase(g.upper(), p.upper()) for p in args.game_ids)]
    if not selected:
        sys.exit("No matching game folders on console")
    total_files = total_size = 0
    for game in selected:
        print(f"{game}  {ps3.game_title(game)}")
        files = list(ps3.walk(game))
        for path, size in files:
            print(f"  {fmt_size(size):>9}  {path}")
        size = sum(s for _, s in files)
        print(f"  {len(files)} file(s), {fmt_size(size)}\n")
        total_files += len(files)
        total_size += size
    if len(selected) > 1:
        print(f"{total_files} file(s), {fmt_size(total_size)}")


def cmd_get(ps3, args):
    for remote, is_dir, size in expand_all(ps3, args.remote):
        if is_dir:
            # keep the folder structure below (and including) the matched folder
            base = posixpath.dirname(remote)
            todo = list(ps3.walk(remote))
        else:
            base = posixpath.dirname(remote)
            todo = [(remote, size)]
        for path, fsize in todo:
            rel = posixpath.relpath(path, base) if is_dir else posixpath.basename(path)
            local = os.path.join(args.output, *rel.split("/"))
            if os.path.exists(local) and not args.force:
                print(f"Skipping {rel}: {local} already exists (use -f to overwrite)")
                continue
            os.makedirs(os.path.dirname(local) or ".", exist_ok=True)
            ps3.download(path, local, fsize)


def cmd_put(ps3, args):
    for local in args.local:
        pkg = open_pkg(local)
        if pkg is None:
            continue
        with pkg:
            files = pkg.files
            total = sum(f.size for f in files)
            print(f"{os.path.basename(local)} -> {pkg.game_id}/  "
                  f"({pkg.content_id}, {len(files)} file(s), {fmt_size(total)})")
            # Only check for existing files in the folders this pkg touches
            existing = {}
            def remote_names(d):
                if d not in existing:
                    try:
                        existing[d] = {n for n, _, _ in ps3.listdir(d)} if ps3.exists_dir(d) else set()
                    except ftplib.error_perm:
                        existing[d] = set()
                return existing[d]
            for item in pkg.items:
                remote = f"{pkg.game_id}/{item.name}"
                if item.is_dir:
                    ps3.makedirs(remote)
                    continue
                rdir, rname = posixpath.split(remote)
                if rname in remote_names(rdir):
                    if not item.overwrite:
                        print(f"  {item.name}: already on console, keeping existing")
                        continue
                    if not args.force:
                        print(f"  {item.name}: already on console, skipping (use -f to overwrite)")
                        continue
                ps3.makedirs(rdir)
                ps3.upload(pkg.open_item(item), remote, item.size)
                remote_names(rdir).add(rname)


def cmd_rm(ps3, args):
    files, dirs = [], []
    for remote, is_dir, size in expand_all(ps3, args.remote):
        if is_dir:
            if not args.recursive:
                print(f"Skipping {remote}: is a folder (use -r to delete it and its contents)")
                continue
            files.extend(ps3.walk(remote))
            # collect subfolders deepest first so they're empty when removed
            sub = [remote]
            stack = [remote]
            while stack:
                d = stack.pop()
                for name, child_dir, _ in ps3.listdir(d):
                    if child_dir:
                        sub.append(posixpath.join(d, name))
                        stack.append(posixpath.join(d, name))
            dirs.extend(sorted(sub, key=lambda p: -p.count("/")))
        else:
            files.append((remote, size))
    if not files and not dirs:
        return
    print("Will delete:")
    for path, size in files:
        print(f"  {fmt_size(size):>9}  {path}")
    for d in dirs:
        print(f"  {'folder':>9}  {d}/")
    if not args.yes:
        what = f"{len(files)} file(s)" + (f" and {len(dirs)} folder(s)" if dirs else "")
        total = fmt_size(sum(s for _, s in files))
        if input(f"Delete {what} ({total})? [y/N] ").strip().lower() != "y":
            return
    for path, _ in files:
        ps3.ftp.delete(ps3.wire(path))
        print(f"Deleted {path}")
    for d in dirs:
        ps3.ftp.rmd(ps3.wire(d))
        print(f"Removed {d}/")


def cmd_info(args):
    for local in args.local:
        pkg = open_pkg(local)
        if pkg is None:
            continue
        with pkg:
            files = pkg.files
            kind = "finalized" if pkg.finalized else "non-finalized"
            print(f"{local}\n  {pkg.content_id}  ({kind})\n  -> {pkg.game_id}/")
            if args.files:
                for f in files:
                    note = "" if f.overwrite else "  (kept if present)"
                    print(f"     {fmt_size(f.size):>9}  {f.name}{note}")
            print(f"  {len(files)} file(s), {fmt_size(sum(f.size for f in files))}\n")


def cmd_extract(args):
    pkg = open_pkg(args.local)
    if pkg is None:
        sys.exit(1)
    with pkg:
        root = os.path.join(args.output, pkg.game_id)
        for item in pkg.items:
            dest = os.path.join(root, *item.name.split("/"))
            if item.is_dir:
                os.makedirs(dest, exist_ok=True)
                continue
            os.makedirs(os.path.dirname(dest), exist_ok=True)
            prog = Progress(item.name, item.size)
            reader = pkg.open_item(item)
            with open(dest, "wb") as fd:
                while chunk := reader.read(1 << 20):
                    fd.write(chunk)
                    prog.update(len(chunk))
            prog.finish()
        print(f"Extracted {len(pkg.files)} file(s) to {root}")


def main():
    p = argparse.ArgumentParser(description="Manage PS3 DLC over FTP.")
    sub = p.add_subparsers(dest="cmd", required=True)

    s = sub.add_parser("list", aliases=["ls"], help="list games, or files of given games")
    s.add_argument("game_ids", nargs="*", help="list files of these game IDs (wildcards ok)")

    s = sub.add_parser("get", help="download files or folders")
    s.add_argument("remote", nargs="+", help="e.g. BLUS30463/USRDIR/dlc/some/file")
    s.add_argument("-o", "--output", default=".", help="local folder (default: current)")
    s.add_argument("-f", "--force", action="store_true", help="overwrite local files")

    s = sub.add_parser("put", help="extract .pkg contents to the console")
    s.add_argument("local", nargs="+")
    s.add_argument("-f", "--force", action="store_true", help="overwrite files on console")

    s = sub.add_parser("rm", help="delete files from the console")
    s.add_argument("remote", nargs="+")
    s.add_argument("-r", "--recursive", action="store_true", help="allow deleting folders")
    s.add_argument("-y", "--yes", action="store_true", help="don't ask for confirmation")

    s = sub.add_parser("info", help="show metadata of local .pkg files")
    s.add_argument("local", nargs="+")
    s.add_argument("-l", "--files", action="store_true", help="list contained files")

    s = sub.add_parser("extract", help="extract a .pkg to a local folder")
    s.add_argument("local")
    s.add_argument("-o", "--output", default=".", help="local folder (default: current)")

    args = p.parse_args()
    if args.cmd == "info":
        return cmd_info(args)
    if args.cmd == "extract":
        return cmd_extract(args)
    ps3 = PS3()
    try:
        {"list": cmd_list, "ls": cmd_list, "get": cmd_get,
         "put": cmd_put, "rm": cmd_rm}[args.cmd](ps3, args)
    finally:
        ps3.close()


if __name__ == "__main__":
    try:
        main()
    except KeyboardInterrupt:
        sys.exit("\nInterrupted")
