#include <GL/glut.h> // Linux, Windows
#include <ft2build.h>
#include <stdio.h>
#include FT_FREETYPE_H

#define PI 3.14159

FT_Library library; /* handle to library */  
FT_Face face; /* handle to face object */  
FT_Matrix matrix;
FT_Vector pen;

char* text = "Text is rendered here.";

int start_char=0;
int end_char=0;
int width = 40;
int height = 500;

void my_draw_bitmap(FT_Bitmap* b, int x, int y) {
  glRasterPos2i(x,y);
  glPushMatrix();
  glScalef(-1,1,1);
  glPixelStorei(GL_UNPACK_ALIGNMENT,1);
  glDrawPixels(b->width, b->rows, GL_LUMINANCE, GL_UNSIGNED_BYTE, b->buffer);

  glPopMatrix();
}

int capture=0;
void keyboard_handler( unsigned char key_id, int x, int y ) {
  switch(key_id) {
  case 'q': exit(0); break;
  case ' ': capture=1; break;
  }
}

void render() {
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  gluOrtho2D(0,width,0,height);
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
  glMatrixMode(GL_MODELVIEW);
  
  float angle = -PI/2;
  matrix.xx = (FT_Fixed)( cos( angle ) * 0x10000L ); 
  matrix.xy = (FT_Fixed)(-sin( angle ) * 0x10000L ); 
  matrix.yx = (FT_Fixed)(-sin( angle ) * 0x10000L ); // ought to be positive
  matrix.yy = (FT_Fixed)( cos( angle ) * 0x10000L );

  //// DRAW SOME TEXT
  FT_GlyphSlot slot = face->glyph; /* a small shortcut */  
  FT_UInt glyph_index; 
  pen.x = width/4 * 64;
  pen.y = (-height+(width/8)) * 64;
  int n;
  for (n = start_char; text[n]!=0 ; n++ ) { 
    FT_Set_Transform( face, &matrix, &pen );
    /* load glyph image into the slot (erase previous one) */  
    int error = FT_Load_Char( face, text[n], FT_LOAD_RENDER ); 
    if ( error ) continue; /* ignore errors */  
    /* now, draw to our target surface */
    if(slot->bitmap_top && (-slot->bitmap_top - (slot->advance.y>>6)) < 0) break;
    my_draw_bitmap( &slot->bitmap,  slot->bitmap_left, -slot->bitmap_top ); 
    /* increment pen position */  
    pen.x += slot->advance.x; pen.y += slot->advance.y;
  }
  end_char=n;
  
  glutSwapBuffers(); // implicitly makes sure drawing completes
}

char stem[] = "textimage";
void scrape(int frame) {
  char name[255]; sprintf(name,"%s%d.pgm",stem,frame);
  FILE* out = fopen(name,"w");
  fprintf(out,"P2\n%d %d\n255\n",width,height);
  GLubyte pixels[height][width];
  glReadPixels(0,0,width,height,GL_LUMINANCE,GL_UNSIGNED_BYTE,pixels);
  int i,j,n=0;
  for(i=height-1;i>=0;i--) {
    for(j=0;j<width;j++) {
      fprintf(out,"%d ",255-pixels[i][j]);
      n++; if(n%10==0) fprintf(out,"\n");
    }
  }
  fclose(out);
}

int frame = 0;
void idle() {
  if(capture && text[end_char]) {
    render(); render();
    scrape(frame++);
    start_char=end_char;
  }
}

char *check_cmd_key_value (int i, int argc, char *argv[]) {
  if (i >= argc) {
    printf("BAD CMD ARGS MISSING KEY VALUE %s\n", argv[i-1]);
    exit(4);
  }
  return argv[i];
}

char fontname[] = "/usr/share/fonts/truetype/ttf-bitstream-vera/Vera.ttf";
int out_set = 0;
void parse_args(int argc,char* argv[]) {
  int i;
  for (i = 1; i < argc; ) {
    char *arg = argv[i++];
    if(strcmp(arg, "-f") == 0) { // which font?
      strcpy(fontname, check_cmd_key_value(i++, argc, argv));
    } else if(strcmp(arg, "-o") == 0) { // which font?
      strcpy(stem, check_cmd_key_value(i++, argc, argv));
      out_set=1;
    } else if(strcmp(arg, "-h") == 0) { // font height
      width = 2*atoi(check_cmd_key_value(i++, argc, argv));
    } else { // file for the text to come from
      FILE* src = fopen(arg,"r");
      // first, see how big the file is
      fseek(src, 0, SEEK_END);
      int size = ftell(src);
      rewind(src);
      // then read it all in
      text = malloc(size+1);
      int result = fread(text,1,size,src);
      if(result!=size) { printf("Couldn't read text file.\n"); exit(5); }
      text[size]=0; // make sure it's terminated
      fclose(src);
      // strip out non-printing characters
      int j;
      for(j=0;j<size;j++) { if(!isprint(text[j])) text[j] = ' '; }
      if(!out_set) strcpy(stem,arg);
    }
  }
}

int main(int argc,char* argv[]) {
  parse_args(argc,argv);
  int error;

  //// FIRST: GET FONT
  error = FT_Init_FreeType( &library ); 
  if ( error ) { printf("Cannot initialize FreeType\n"); exit(1); }

  error = FT_New_Face(library, fontname, 0, &face );
  if(error == FT_Err_Unknown_File_Format ) { 
    printf("Cannot open font file\n"); exit(2);
  } else if (error) { 
    printf("Cannot open font file\n"); exit(3);
  }

  error = FT_Set_Char_Size( face, /* handle to face object */  
                            0, /* char_width in 1/64th of points */  
                            width/2*64, /* char_height in 1/64th of points */  
                            72, /* horizontal device resolution */  
                            72 ); /* vertical device resolution */

  //// SECOND: SETUP OPENGL DRAWING
  glutInitWindowSize(width, height); // default size
  glutInitWindowPosition (100, 100); // default position
  glutInitDisplayMode (GLUT_DEPTH | GLUT_ACCUM | GLUT_RGBA | GLUT_DOUBLE);
  glutInit(&argc, argv); // allows user override of geometry
  int window = glutCreateWindow("Render"); // title window with cmd line
  glClear(GL_ACCUM_BUFFER_BIT | GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
  glClearColor(0,0,0,1);
  glLineWidth(1);
  glPointSize(1);
  glEnable(GL_DEPTH_TEST);	// Enables Depth Testing
  glDepthFunc(GL_LEQUAL);	// The Type Of Depth Testing (Less Or Equal)
  glClearDepth(1.0f);		// Depth Buffer Setup
  glEnable(GL_LINE_SMOOTH);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glHint(GL_LINE_SMOOTH_HINT, GL_DONT_CARE);
  glShadeModel (GL_SMOOTH);	// Select Smooth Shading
  glEnable(GL_TEXTURE_2D);
  glHint (GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);
  // select modulate to mix texture with color for shading
  glTexEnvf( GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE );
  glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_LINEAR);

  glutDisplayFunc(render);
  glutIdleFunc(idle);
  glutKeyboardFunc(keyboard_handler);
  
  glutMainLoop(); 

  printf("Successfully got to end.\n");

}
