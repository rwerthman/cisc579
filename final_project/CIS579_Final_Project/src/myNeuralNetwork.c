#include "inc/myNeuralNetwork.h"

#define num_outputs 3
#define num_inputs 3
#define learning_rate 0.5

//float training_data_input[9] = {
//  {0, 1, 1}, // If alien is above spacecraft and there is no bullet coming drop bomb
//  {1, 1, 1}, // If alien is above spacecraft and there is a bullet avoid it by moving left or right
//  {1, 0, 1}, // If alien is above bullet move left or right
//  {0, 0, 1}, // If alien isn't above anything move left or right
//};
//float traning_data_output[3] = {
//  {0, 0, 1}, // Drop bomb
//  {1, 0, 0}, // Move left
//  {0, 1, 0}, // Move right
//  {0, 1, 0}, // Move right
//};




/* input is the x value of the bullet, x value of alien, x value of the spacecraft */
//uint8_t input[num_inputs];

/* output is the
 *  index 0: move left,
 *  index 1: move right,
 *  index 2: fire bomb */
//uint8_t output[num_outputs];

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

void calculate_neural_network_outputs(float output[], uint8_t input[])
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

void backpropagate_error_to_weights(uint8_t input[], float output[], uint8_t target_outputs[])
{
  uint8_t i, j;
  float error[num_outputs];
  // Calculate the error for each output error = (expected output - real output)
  for (i = 0; i < num_outputs; i++)
  {
    error[i] = target_outputs[i] - output[i];

    error[i] = error[i] * sigmoid_function(output[i]) * (1 - sigmoid_function(output[i]));
  }

  // Calculate dot product with the weight matrix transposed
  for (i = 0; i < num_inputs; i++)
  {
    for (j = 0; j < num_outputs; j++)
    {
      weights_input_to_output[j][i] += learning_rate * error[j] * input[i];
    }
  }

}

float sigmoid_function(float x)
{
  return 1/(1 + expf(-x));
}

void generate_training_data(uint16_t num_training_samples)
{
  uint16_t i;
  uint8_t x, y, z;

  uint8_t training_input[3];
  uint8_t training_output[3];
  float real_output[3];

  // For the # of training samples
  for (i = 0; i < num_training_samples; i++)
  {
    // Randomly choose the x locations of the bullet, alien, and spacecraft
    x = rand() % 127;
    y = rand() % 127;
    z = rand() % 127;

    training_input[0] = x;
    training_input[1] = y;
    training_input[2] = z;

    // Base on the training input decide what the alien should do
    // x = bullet, y = spacecraft, z = alien
    if (x == z)
    {
      // If bullet has same x as alien move alien
      // Spaceship can move left or right
      training_output[rand() % 2] = 1;
    }
    else if (z == y && x != z)
    {
      // If the alien is in the same x as spacecraft and bullet not in same x
      // Alien drops bomb on spaceshipt
      training_output[2] = 1;
    }
    else if (x == y && z == y)
    {
      // If the alien is in the same x as spacecraft and bullet in same x
      // Move alien left or right
      training_output[rand() % 2] = 1;
    }
    else if (z != x && z != y)
    {
      // If the alien is not above anything move left or right
      training_output[rand() % 2] = 1;
    }

    calculate_neural_network_outputs(real_output, training_input);
    backpropagate_error_to_weights(training_input, real_output, training_output);

  }

}


