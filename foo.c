#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <unistd.h>
#include <jack/jack.h>

jack_port_t *output_port1, *output_port2;
jack_client_t *client;

float phase = 0;

float pi = 3.14159265358979323846264338327950288419716939937;

float clamp(float input) {
  if (input >=  2 * pi) {
    return clamp(input - 2 * pi);
  } else {
    return input;
  }
}

int process (jack_nframes_t nframes, void *arg) {
	jack_default_audio_sample_t *out1, *out2;

	out1 = (jack_default_audio_sample_t*) jack_port_get_buffer (output_port1, nframes);
	out2 = (jack_default_audio_sample_t*) jack_port_get_buffer (output_port2, nframes);


	int i;
	for( i=0; i<nframes; i++ ) {
    phase = phase + (2 * pi / 48000);
    phase = clamp(phase);
    float output = sin(phase * 440);
		out1[i] = output;
		out2[i] = output;
	}

	return 0;
}

/**
 * JACK calls this shutdown_callback if the server ever shuts down or
 * decides to disconnect the client.
 */
void
jack_shutdown (void *arg)
{
	exit (1);
}

int main (int argc, char *argv[]) {
	const char *client_name = "foo";
	const char *server_name = NULL;
	jack_options_t options = JackNullOption;
	jack_status_t status;

	/* open a client connection to the JACK server */

	client = jack_client_open (client_name, options, &status, server_name);
	if (client == NULL) {
		fprintf (stderr, "jack_client_open() failed, "
			 "status = 0x%2.0x\n", status);
		if (status & JackServerFailed) {
			fprintf (stderr, "Unable to connect to JACK server\n");
		}
		exit (1);
	}

	if (status & JackServerStarted) {
		fprintf (stderr, "JACK server started\n");
	}

	if (status & JackNameNotUnique) {
		client_name = jack_get_client_name(client);
		fprintf (stderr, "unique name `%s' assigned\n", client_name);
	}

	jack_set_process_callback (client, process, NULL);

	jack_on_shutdown (client, jack_shutdown, 0);

	output_port1 = jack_port_register (client, "output1",
					  JACK_DEFAULT_AUDIO_TYPE,
					  JackPortIsOutput, 0);

	output_port2 = jack_port_register (client, "output2",
					  JACK_DEFAULT_AUDIO_TYPE,
					  JackPortIsOutput, 0);

	if ((output_port1 == NULL) || (output_port2 == NULL)) {
		fprintf(stderr, "no more JACK ports available\n");
		exit (1);
	}

	if (jack_activate (client)) {
		fprintf (stderr, "cannot activate client");
		exit (1);
	}

	while (1) {
		sleep (1);
	}
}
