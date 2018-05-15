/*
 * Mathmatical functions needed:
 *    Dot product
 *    Matrix Transpose
 *    Sigmoid -> activation function
 *
 *  Variables needed:
 *    Inputs -> array
 *    Outputs -> array
 *    Weights -> multidimensional array
 *    Error for each layer
 */
#include "inc/myNeuralNetwork.h"

#define num_outputs 3
#define num_inputs 2
#define learning_rate 0.5

/* input is the x value of the bullet and x value of the spacecraft */
uint8_t input[num_inputs];

/* output is the
 *  index 0: move left,
 *  index 1: move right,
 *  index 2: fire bomb */
uint8_t output[num_outputs];

float weights_input_to_output[num_outputs][num_inputs];

float sigmoid_function(float x);

// https://stackoverflow.com/questions/4310277/producing-random-float-from-negative-to-positive-range
void initialize_neural_network_weights()
{
  uint8_t i, j;
  for (i = 0; i < num_outputs; i++)
  {
    for (j = 0; j < num_inputs; j++)
    {
      // Weights are initialized to 1/sqrt(number of links to nodes) in position to negative range
      weights_input_to_output[i][j] = (((float)rand() / RAND_MAX) * (2*(1.0/sqrtf(num_inputs)))) - 1.0/sqrtf(num_inputs);
    }
  }

}

void calculate_nueral_network_outputs(float outputs[])
{
  uint8_t i, j;

  // Calculate dot product of the weights and input
  for (i = 0; i < num_outputs; i++)
  {
    for (j = 0; j < num_inputs; j++)
    {
      output[i] += weights_input_to_output[i][j] * input[j];
    }
  }

  // Put the output through activation function
  for (i = 0; i < num_outputs; i++)
  {
    output[i] = sigmoid_function(output[i]);
  }
}

void backpropagate_error_to_weights()
{

}

float sigmoid_function(float x)
{
  return 1/(1 + expf(-x));
}


