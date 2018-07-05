def write_start(file):
  file.write('#include "tensors.xh"\n')
  file.write('\n')
  file.write('tensor format s0s1 = ({sparse, sparse}, {0, 1});\n')
  file.write('tensor format d0s1 = ({dense, sparse}, {0, 1});\n')
  file.write('tensor format s0d1 = ({sparse, dense}, {0, 1});\n')
  file.write('tensor format d0d1 = ({dense, dense}, {0, 1});\n')
  file.write('tensor format s1s0 = ({sparse, sparse}, {1, 0});\n')
  file.write('tensor format d1s0 = ({dense, sparse}, {1, 0});\n')
  file.write('tensor format s1d0 = ({sparse, dense}, {1, 0});\n')
  file.write('tensor format d1d0 = ({dense, dense}, {1, 0});\n')
  file.write('\n')
  file.write('double result[] = {0.0, 0.25, 0.0, 0.0, 0.0, 0.0, 3.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0};\n')
  file.write('\n')
  file.write('char err = 0;\n')
  file.write('\n')
  file.write('template<a, b, c>\n')
  file.write('void compute(a A, b B, c C, int id) {\n')
  file.write('  tensor A(i,j) = B(i,j) * C(i,j);\n')
  file.write('\n')
  file.write('  for(int i = 0; i < 4; i++) {\n')
  file.write('    for(int j = 0; j < 4; j++) {\n')
  file.write('      double val = value(A)(i,j);\n')
  file.write('      double exp = result[i * 4 + j];\n')
  file.write('      if(val != exp) {\n')
  file.write('        fprintf(stderr, "Assert %d failed (id %d). Got %f, expected %f.\\n", i * 4 + j + 1, id, val, exp);\n')
  file.write('        err = 1;\n')
  file.write('      }\n')
  file.write('    }\n')
  file.write('  }\n')
  file.write('}\n')
  file.write('\n')
  file.write('template<a>\n')
  file.write('void fillB(a B) {\n')
  file.write('  value(B)(0,1) = 0.5;\n')
  file.write('  value(B)(0,2) = 1.0;\n')
  file.write('  value(B)(1,0) = 1.5;\n')
  file.write('  value(B)(1,2) = 2.0;\n')
  file.write('  value(B)(1,3) = 2.5;\n')
  file.write('  value(B)(3,0) = 0.5;\n')
  file.write('}\n')
  file.write('\n')
  file.write('template<a>\n')
  file.write('void fillC(a C) {\n')
  file.write('  value(C)(0,1) = 0.5;\n')
  file.write('  value(C)(1,1) = 1.0;\n')
  file.write('  value(C)(1,2) = 1.5;\n')
  file.write('  value(C)(3,0) = 2.0;\n')
  file.write('  value(C)(3,1) = 2.5;\n')
  file.write('  value(C)(3,3) = 0.5;\n')
  file.write('}\n')
  file.write('\n')
  file.write('template<a>\n')
  file.write('a makeA() {\n')
  file.write('  return build(a)({4, 4});\n')
  file.write('}\n')
  file.write('\n')
  file.write('template<a>\n')
  file.write('a makeB() {\n')
  file.write('  a res = build(a)({4, 4});\n')
  file.write('  inst fillB<a>(res);\n')
  file.write('  return res;\n')
  file.write('}\n')
  file.write('\n')
  file.write('template<a>\n')
  file.write('a makeC() {\n')
  file.write('  a res = build(a)({4, 4});\n')
  file.write('  inst fillC<a>(res);\n')
  file.write('  return res;\n')
  file.write('}\n')
  file.write('\n')
  file.write('int main() {\n')

options = [(0, 1, 0, 1, 0, 1), (1, 0, 1, 0, 1, 0)]
formats = ["s%ds%d", "s%dd%d", "d%ds%d", "d%dd%d"]

def write_main(files):
  count = 0
  file = 0
  for o in options:
    for fa in formats:
      for fb in formats:
        for fc in formats:
          a = fa % (o[0], o[1])
          b = fb % (o[2], o[3])
          c = fc % (o[4], o[5])
          files[count // 4].write('  inst compute<tensor<%s>, tensor<%s>, tensor<%s>>(inst makeA<tensor<%s>>(), inst makeB<tensor<%s>>(), inst makeC<tensor<%s>>(), %d);\n' % (a, b, c, a, b, c, count + 1))
          count = count + 1

def write_end(files):
  for file in files:
    file.write('  if(err) exit(1);\n')
    file.write('\n')
    file.write('  return 0;\n')
    file.write('}\n')

files = []
for i in range(0, 32):
  files.append(open("expr_gen_01_%02d.xc" % (i+1), "w"))
  write_start(files[i])

write_main(files)
write_end(files)

