
/*
  build instructions

  > gcc -o atlas atlas.c -lSDL2 -lSDL2_ttf
  > ./atlas

*/

#include <SDL2/SDL.h>
#include <SDL2/SDL_ttf.h>
#include <stdio.h>
#include <ctype.h>  // for isprint

#define ATLAS_WIDTH  512
#define ATLAS_HEIGHT 512

typedef struct {
    SDL_Rect src;   // source rectangle in atlas
    int advance;    // horizontal advance
} Glyph;

Glyph glyphs[128]; // store ASCII 0–127

int main(int argc, char *argv[]) {
    if (SDL_Init(SDL_INIT_VIDEO) != 0) {
        printf("SDL_Init Error: %s\n", SDL_GetError());
        return 1;
    }

    if (TTF_Init() != 0) {
        printf("TTF_Init Error: %s\n", TTF_GetError());
        SDL_Quit();
        return 1;
    }

    SDL_Window *win = SDL_CreateWindow("SDL2 Glyph Atlas",
                                       SDL_WINDOWPOS_CENTERED,
                                       SDL_WINDOWPOS_CENTERED,
                                       800, 600, SDL_WINDOW_SHOWN);
    if (!win) {
        printf("SDL_CreateWindow Error: %s\n", SDL_GetError());
        TTF_Quit();
        SDL_Quit();
        return 1;
    }

    SDL_Renderer *ren = SDL_CreateRenderer(win, -1,
                                           SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC);
    if (!ren) {
        printf("SDL_CreateRenderer Error: %s\n", SDL_GetError());
        SDL_DestroyWindow(win);
        TTF_Quit();
        SDL_Quit();
        return 1;
    }

    TTF_Font *font = TTF_OpenFont("/usr/share/fonts/truetype/jetbrains-mono/fonts/ttf/JetBrainsMono-Regular.ttf", 16);
    if (!font) {
        printf("TTF_OpenFont Error: %s\n", TTF_GetError());
        SDL_DestroyRenderer(ren);
        SDL_DestroyWindow(win);
        TTF_Quit();
        SDL_Quit();
        return 1;
    }

    // Create the atlas texture
    SDL_Texture *atlas = SDL_CreateTexture(ren, SDL_PIXELFORMAT_RGBA8888,
                                           SDL_TEXTUREACCESS_TARGET, ATLAS_WIDTH, ATLAS_HEIGHT);
    SDL_SetTextureBlendMode(atlas, SDL_BLENDMODE_BLEND);

    SDL_SetRenderTarget(ren, atlas);
    SDL_SetRenderDrawColor(ren, 0, 0, 0, 0);
    SDL_RenderClear(ren);

    int x = 0, y = 0, row_height = 0;

    for (char c = 32; c < 127; ++c) {
        SDL_Surface *surf = TTF_RenderGlyph_Blended(font, c, (SDL_Color){255,255,255,255});
        if (!surf) continue;

        SDL_Texture *tex = SDL_CreateTextureFromSurface(ren, surf);
        if (!tex) {
            SDL_FreeSurface(surf);
            continue;
        }

        if (x + surf->w > ATLAS_WIDTH) {
            x = 0;
            y += row_height;
            row_height = 0;
        }

        SDL_Rect dst = {x, y, surf->w, surf->h};
        SDL_RenderCopy(ren, tex, NULL, &dst);

        int minx, maxx, miny, maxy, advance;
        if (TTF_GlyphMetrics(font, c, &minx, &maxx, &miny, &maxy, &advance) == 0) {
            glyphs[(int)c].src = dst;
            glyphs[(int)c].advance = advance;
        }

        x += surf->w;
        if (surf->h > row_height) row_height = surf->h;

        SDL_DestroyTexture(tex);
        SDL_FreeSurface(surf);
    }

    SDL_SetRenderTarget(ren, NULL);

    int quit = 0;
    SDL_Event e;

    while (!quit) {
        while (SDL_PollEvent(&e)) {
            if (e.type == SDL_QUIT) quit = 1;
        }

        SDL_SetRenderDrawColor(ren, 0, 0, 0, 255);
        SDL_RenderClear(ren);

        // Draw text from atlas
        const char *text = "Hello, SDL2 Glyph Atlas!";
        int pen_x = 50, pen_y = 50;

        for (const char *p = text; *p; ++p) {
            char c = *p;
            if (c < 32 || c > 126) continue;

            SDL_Rect src = glyphs[(int)c].src;
            SDL_Rect dst = {pen_x, pen_y, src.w, src.h};
            SDL_RenderCopy(ren, atlas, &src, &dst);
            pen_x += glyphs[(int)c].advance;
        }

        SDL_RenderPresent(ren);
        SDL_Delay(16);
    }

    SDL_DestroyTexture(atlas);
    TTF_CloseFont(font);
    SDL_DestroyRenderer(ren);
    SDL_DestroyWindow(win);
    TTF_Quit();
    SDL_Quit();



// Print atlas glyphs as S-expression
printf("(\n");
for (char c = 32; c < 127; ++c) {
    SDL_Rect r = glyphs[(int)c].src;
    int adv = glyphs[(int)c].advance;

    // Escape characters if needed
    if (c == '\\' || c == '"') {
        // skip for now
        continue;
    }

    // Use #\c notation
    printf("  (CHAR #\\%c :x %d :y %d :w %d :h %d :advance %d)\n",
           c, r.x, r.y, r.w, r.h, adv);
}
printf(")\n");
    
    return 0;
}

