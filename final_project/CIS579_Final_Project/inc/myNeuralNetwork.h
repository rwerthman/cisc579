#ifndef INC_MYNEURALNETWORK_H_
#define INC_MYNEURALNETWORK_H_

#include <stdint.h>
#include <stdlib.h>
#include "math.h"

#define num_outputs 1 // Number of output nodes
#define num_hidden 3 // Number of hidden layer nodes
#define num_inputs 3 // Number of input nodes

void initialize_neural_network_weights();
void calculate_neural_network_outputs(float output[num_outputs], uint8_t input[num_inputs]);
void backpropagate_error_to_weights(uint8_t input[num_inputs], float output[num_outputs], uint8_t target_outputs[num_outputs]);
void generate_training_data(uint16_t num_training_samples);



#endif /* INC_MYNEURALNETWORK_H_ */
