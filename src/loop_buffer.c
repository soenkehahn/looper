#include <stdio.h>
#include <stdlib.h>
#include <jack/jack.h>

jack_port_t *output_port1, *output_port2;

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

// jack stuff

int process(jack_nframes_t nframes, void* arg) {
  struct loopnaut* loopnaut = arg;

  jack_default_audio_sample_t* out1, * out2;
  out1 = (jack_default_audio_sample_t*) jack_port_get_buffer(output_port1, nframes);
  out2 = (jack_default_audio_sample_t*) jack_port_get_buffer(output_port2, nframes);

  for(int i = 0; i < nframes; i++) {
    float output = get_next_sample(loopnaut);
    out1[i] = output;
    out2[i] = output;
  }

  return 0;
}

void jack_shutdown(void *arg) {
  fprintf(stderr, "jack_shutdown");
  exit(1);
}

jack_client_t* init_client() {
  const char *client_name = "loopnaut";
  const char *server_name = NULL;
  jack_options_t options = JackNullOption;
  jack_status_t status;

  jack_client_t *client;
  client = jack_client_open(client_name, options, &status, server_name);
  if (client == NULL) {
    fprintf(stderr, "jack_client_open() failed, status = 0x%2.0x\n", status);
    if (status & JackServerFailed) {
      fprintf(stderr, "Unable to connect to JACK server\n");
    }
    exit(1);
  }

  if (status & JackServerStarted) {
    fprintf(stderr, "JACK server started\n");
  }

  if (status & JackNameNotUnique) {
    client_name = jack_get_client_name(client);
    fprintf(stderr, "unique name `%s' assigned\n", client_name);
  }

  jack_on_shutdown(client, jack_shutdown, 0);

  return client;
}

struct loopnaut* create_loopnaut() {
  struct loopnaut* loopnaut = create_empty_loopnaut();

  jack_client_t* client = init_client();

  jack_set_process_callback(client, process, loopnaut);

  output_port1 = jack_port_register(client, "output1",
    JACK_DEFAULT_AUDIO_TYPE,
    JackPortIsOutput, 0);

  output_port2 = jack_port_register(client, "output2",
    JACK_DEFAULT_AUDIO_TYPE,
    JackPortIsOutput, 0);

  if ((output_port1 == NULL) || (output_port2 == NULL)) {
    fprintf(stderr, "no more JACK ports available\n");
    exit(1);
  }

  if (jack_activate(client)) {
    fprintf(stderr, "cannot activate client\n");
    exit(1);
  }

  return loopnaut;
}
