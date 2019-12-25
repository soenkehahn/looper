#include <portaudio.h>
#include <stdio.h>
#include <stdlib.h>

struct loopnaut {
  float* array;
  int index;
  int length;
  struct loopnaut* next;
};

struct loopnaut* create_empty_loopnaut() {
  struct loopnaut* result = malloc(sizeof(struct loopnaut));
  float array[1] = {0.0};
  result->array = array;
  result->index = 0;
  result->length = 1;
  result->next = NULL;
  return result;
}

void set_buffer(struct loopnaut* loopnaut, float* array, int length) {
  struct loopnaut* next = malloc(sizeof(struct loopnaut));
  next->array = array;
  next->index = 0;
  next->length = length;
  next->next = NULL;
  loopnaut->next = next;
}

float get_next_sample(struct loopnaut* loopnaut) {
  float result = loopnaut->array[loopnaut->index];
  loopnaut->index++;
  if (loopnaut->index >= loopnaut->length) {
    if (loopnaut->next == NULL) {
      loopnaut->index = 0;
    } else {
      loopnaut->array = loopnaut->next->array;
      loopnaut->index = loopnaut->next->index;
      loopnaut->length = loopnaut->next->length;
      loopnaut->next = NULL;
    }
  }
  return result;
}

void start_portaudio(struct loopnaut* loopnaut);

struct loopnaut* create_loopnaut() {
  struct loopnaut* loopnaut = create_empty_loopnaut();
  start_portaudio(loopnaut);
  return loopnaut;
}

// portaudio stuff

static int paCallback(
  const void* inputBuffer,
  void* outputBuffer,
  unsigned long framesPerBuffer,
  const PaStreamCallbackTimeInfo* timeInfo,
  PaStreamCallbackFlags statusFlags,
  void* userdata
) {
  struct loopnaut* loopnaut = (struct loopnaut*) userdata;
  float* out = (float*) outputBuffer;

  for(unsigned long i = 0; i < framesPerBuffer; i++) {
    float sample = get_next_sample(loopnaut);
    out[0] = sample; // left
    out[1] = sample; // right
    out += 2;
  }

  return paContinue;
}

void start_portaudio(struct loopnaut* loopnaut) {
  PaStreamParameters outputParameters;
  PaStream* stream;
  PaError err;

  err = Pa_Initialize();
  if(err != paNoError) goto error;

  outputParameters.device = Pa_GetDefaultOutputDevice();
  if (outputParameters.device == paNoDevice) {
    fprintf(stderr,"Error: No default output device.\n");
    goto error;
  }
  outputParameters.channelCount = 2;
  outputParameters.sampleFormat = paFloat32;
  outputParameters.suggestedLatency =
    Pa_GetDeviceInfo(outputParameters.device)->defaultLowOutputLatency;
  outputParameters.hostApiSpecificStreamInfo = NULL;

  err = Pa_OpenStream(
    &stream,
    NULL,
    &outputParameters,
    44100,
    1024,
    paClipOff,
    paCallback,
    loopnaut
  );
  if(err != paNoError) goto error;

  err = Pa_StartStream(stream);
  if(err != paNoError) goto error;

  return;

error:
  Pa_Terminate();
  fprintf(stderr, "An error occured while using the portaudio stream\n");
  fprintf(stderr, "Error number: %d\n", err);
  fprintf(stderr, "Error message: %s\n", Pa_GetErrorText(err));
}
