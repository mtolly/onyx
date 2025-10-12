#!/usr/bin/env python3
import os
import re
import sys

# Claude Code wrote this to make use of BlockArguments to remove dollar signs in Haskell code

def add_block_arguments_pragma(content):
    """Add BlockArguments pragma if not already present"""
    lines = content.split('\n')
    
    # Check if BlockArguments is already present
    for line in lines:
        if re.match(r'^\s*{-#\s*LANGUAGE.*BlockArguments.*#-}', line):
            return content
    
    # Find where to insert the pragma
    pragma_inserted = False
    new_lines = []
    
    for i, line in enumerate(lines):
        # (MT: disabled)
        # If we find an existing LANGUAGE pragma, add BlockArguments to it
        if False and re.match(r'^\s*{-#\s*LANGUAGE\s+', line) and not pragma_inserted:
            # Check if this is a single-line pragma
            if '#-}' in line:
                # Single line pragma - add BlockArguments
                pragma_part = line.split('#-}')[0]
                if 'BlockArguments' not in pragma_part:
                    new_line = pragma_part + ', BlockArguments #-}'
                    new_lines.append(new_line)
                    pragma_inserted = True
                else:
                    new_lines.append(line)
            else:
                # Multi-line pragma start
                new_lines.append(line)
        # (MT: disabled)
        # If this is the end of a multi-line pragma, add BlockArguments
        elif False and '#-}' in line and i > 0 and any(re.match(r'^\s*{-#\s*LANGUAGE', prev_line) for prev_line in lines[max(0, i-10):i]) and not pragma_inserted:
            if 'BlockArguments' not in line:
                # Add BlockArguments before the closing
                parts = line.split('#-}')
                new_line = parts[0] + ', BlockArguments #-}' + '#-}'.join(parts[1:])
                new_lines.append(new_line)
                pragma_inserted = True
            else:
                new_lines.append(line)
        # If we haven't found any LANGUAGE pragma and this is the module declaration
        elif re.match(r'^\s*module\s+', line) and not pragma_inserted:
            new_lines.append('{-# LANGUAGE BlockArguments #-}')
            new_lines.append(line)
            pragma_inserted = True
        else:
            new_lines.append(line)
    
    # If we still haven't added the pragma and there's no module declaration, add it at the top
    if not pragma_inserted:
        new_lines.insert(0, '{-# LANGUAGE BlockArguments #-}')
    
    return '\n'.join(new_lines)

def apply_block_arguments(content):
    """Apply BlockArguments transformations - only remove $ and one space"""
    
    # Very conservative patterns that only replace $ followed by space with nothing
    # This ensures we only remove the exact "$ " sequence and nothing more
    
    # foo $ \x -> ...  becomes  foo \x -> ...
    content = re.sub(r'(\w+) \$ (\\[^c])', r'\1 \2', content)
    
    # foo $ \case  becomes  foo \case  
    content = re.sub(r'(\w+) \$ (\\case)', r'\1 \2', content)
    
    # foo $ case  becomes  foo case (only when followed by whitespace or end of line)
    content = re.sub(r'(\w+) \$ (case)(\s|$)', r'\1 \2\3', content)
    
    # foo $ if  becomes  foo if (only when followed by whitespace or end of line)
    content = re.sub(r'(\w+) \$ (if)(\s|$)', r'\1 \2\3', content)
    
    # foo $ let  becomes  foo let (only when followed by whitespace or end of line)
    content = re.sub(r'(\w+) \$ (let)(\s|$)', r'\1 \2\3', content)
    
    # foo $ do  becomes  foo do (but only when 'do' is followed by whitespace or end of line)
    content = re.sub(r'(\w+) \$ (do)(\s|$)', r'\1 \2\3', content)
    
    return content

def process_file(filepath):
    """Process a single Haskell file"""
    try:
        with open(filepath, 'r', encoding='utf-8') as f:
            content = f.read()
        
        # Apply transformations
        new_content = apply_block_arguments(content)
        new_content = add_block_arguments_pragma(new_content)
        
        # Only write if content changed
        if new_content != content:
            with open(filepath, 'w', encoding='utf-8') as f:
                f.write(new_content)
            print(f"Updated: {filepath}")
            return True
        else:
            return False
    except Exception as e:
        print(f"Error processing {filepath}: {e}")
        return False

def main():
    if len(sys.argv) != 2:
        print("Usage: python3 apply_block_arguments.py <directory>")
        sys.exit(1)
    
    directory = sys.argv[1]
    updated_count = 0
    total_count = 0
    
    for root, dirs, files in os.walk(directory):
        for file in files:
            if file.endswith('.hs'):
                filepath = os.path.join(root, file)
                total_count += 1
                if process_file(filepath):
                    updated_count += 1
    
    print(f"Processed {total_count} files, updated {updated_count} files")

if __name__ == "__main__":
    main()
