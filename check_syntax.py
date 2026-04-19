def check_lisp_syntax(filepath):
    with open(filepath, 'r') as f:
        content = f.read()
    
    stack = []
    line_no = 1
    col_no = 1
    
    in_string = False
    in_comment = False
    escape = False
    
    for i, char in enumerate(content):
        if char == '\n':
            line_no += 1
            col_no = 1
            if in_comment:
                in_comment = False
        else:
            col_no += 1
            
        if in_comment:
            continue
            
        if in_string:
            if escape:
                escape = False
            elif char == '\\':
                escape = True
            elif char == '"':
                in_string = False
            continue
            
        if char == '"':
            in_string = True
            continue
            
        if char == ';':
            in_comment = True
            continue
            
        if char == '(':
            stack.append((line_no, col_no))
        elif char == ')':
            if not stack:
                print(f"Extra closing parenthesis at Line {line_no}, Col {col_no}")
                return False
            stack.pop()
            
    if stack:
        for l, c in stack:
            print(f"Unclosed parenthesis starting at Line {l}, Col {c}")
        return False
        
    if in_string:
        print("Unclosed string literal")
        return False
        
    print("Syntax parity looks OK.")
    return True

check_lisp_syntax('c:/Users/Geeteshh/fast_qc/dim_qc.lsp')
