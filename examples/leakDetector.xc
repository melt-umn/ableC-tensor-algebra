#include "tensors.xh"

tensor format vec ({sparse});

int main() {
  tensor<vec> vector = build(tensor<vec>)({100});

  for(int i = 0; i < 5; i++) {
    vector[i * 20] = i * i;
  }

  vector[3];

  freeTensor(vector);
}
