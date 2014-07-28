import re
import sys


whitespace_re = re.compile('^\s')

PRELUDE = 0
COMMENT = 1
SEXP = 2


def main():
    state = PRELUDE
    out = []

    for line in sys.stdin:
        if line[0] == ';':
            if state == SEXP:
                sys.stdout.write('```\n\n\n')

            sys.stdout.write(line[3:])
            state = COMMENT
        elif line[0] == '(':
            if state != PRELUDE:
                if state != SEXP:
                    sys.stdout.write('\n\n``` {.clojure .numberLines}\n')
                sys.stdout.write(line)
                state = SEXP
        elif whitespace_re.match(line[0]):
            if state == SEXP:
                sys.stdout.write(line)
            elif state == COMMENT:
                sys.stdout.write('\n')
        else:
            sys.stderr.write('Unknown line: {}'.format(line))

    if state == SEXP:
        sys.stdout.write('```\n')


if __name__ == '__main__':
    main()
