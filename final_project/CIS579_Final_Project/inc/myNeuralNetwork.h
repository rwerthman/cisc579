#ifndef INC_MYNEURALNETWORK_H_
#define INC_MYNEURALNETWORK_H_

#include <stdint.h>
#include <stdlib.h>
#include "math.h"

void initialize_neural_network_weights();
void calculate_neural_network_outputs(float output[], uint8_t input[]);
void backpropagate_error_to_weights(uint8_t input[], float output[], uint8_t target_outputs[]);
void generate_training_data(uint16_t num_training_samples);



#endif /* INC_MYNEURALNETWORK_H_ */
