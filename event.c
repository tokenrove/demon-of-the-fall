
#include "SDL.h"
#include <string.h>

struct ll_event_s {
    int type;
    int keysym;
};

int ll_poll_event(struct ll_event_s* event)
{
    SDL_Event ev;
    int rv;

    rv = SDL_PollEvent(&ev);
    if(rv && ev.type == SDL_KEYDOWN) {
	event->type = 2;
	event->keysym = ev.key.keysym.sym;
    } else if(rv && ev.type == SDL_KEYUP) {
	event->type = 3;
	event->keysym = ev.key.keysym.sym;
    }
    return rv;
}


int ll_wait_event(struct ll_event_s* event)
{
    SDL_Event ev;
    int rv;

    rv = SDL_WaitEvent(&ev);
    if(rv && ev.type == SDL_KEYDOWN) {
	event->type = 2;
	event->keysym = ev.key.keysym.sym;
    } else if(rv && ev.type == SDL_KEYUP) {
	event->type = 3;
	event->keysym = ev.key.keysym.sym;
    }
    return rv;
}
