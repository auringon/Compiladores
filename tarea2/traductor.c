/*********** LIbrerias utilizadas **************/
#include<stdio.h>
#include<string.h>
#include<stdlib.h>
#include<ctype.h>
/***************** MACROS **********************/
#define LITERAL_CADENA		0
#define LITERAL_NUM			1
#define PR_TRUE				2
#define PR_FALSE			3
#define PR_NULL				4
#define L_CORCHETE 			5
#define R_CORCHETE 			6
#define L_LLAVE  			7
#define R_LLAVE  			8
#define COMA	 			9
#define DOS_PUNTOS			10
#define EMPTY				11

#define TAMBUFF 	5
#define TAMLEX 		100
#define TAMHASH 	151

/************* Definiciones ********************/
typedef struct entrada{
	//definir los campos de 1 entrada de la tabla de simbolos
	int compLex;
	char lexema[TAMLEX];	
	struct entrada *tipoDato; 	
} entrada;

typedef struct {
	int compLex;
	entrada *pe;
} token;

/************* Tokens ********************/
char *tokens[]={"LITERAL_CADENA",
			"LITERAL_NUM",
			"PR_TRUE",
			"PR_FALSE",
			"PR_NULL",
			"L_CORCHETE",
			"R_CORCHETE",
			"L_LLAVE",
			"R_LLAVE",
			"COMA",
			"DOS_PUNTOS", 
			"EMPTY"};



/************* Conjuntos primeros y segundos ********************/
char first[11][5]=
		{
			{},
			{L_CORCHETE, LITERAL_CADENA},
			{LITERAL_CADENA},
			{L_LLAVE},
			{LITERAL_CADENA},
			{COMA, EMPTY},
			{LITERAL_CADENA},	
			{LITERAL_CADENA},			
			{LITERAL_CADENA, LITERAL_NUM, PR_TRUE, PR_FALSE,PR_NULL},
			{L_CORCHETE, LITERAL_CADENA},			
			{COMA, EMPTY}	
		};	
char next[11][3]=		
	{
			{},
			{EOF, COMA,R_CORCHETE},
			{COMA, R_CORCHETE},
			{LITERAL_CADENA},
			{R_LLAVE},
			{R_LLAVE},	
			{COMA,R_LLAVE},			
			{DOS_PUNTOS},
			{COMA, R_LLAVE},			
			{R_CORCHETE},
			{R_CORCHETE}	
		};	



/************* Variables globales **************/
int tabcounter = 0;		//Cuenta la tabulacion  
int consumir;			/* 1 indica al analizador lexico que debe devolver
						el sgte componente lexico, 0 debe devolver el actual */

char cad[5*TAMLEX];		// string utilizado para cargar mensajes de error
token t;				// token global para recibir componentes del Analizador Lexico

FILE *archivo;			// Fuente a leer
char buff[2*TAMBUFF];	// Buffer para lectura de archivo fuente
char id[TAMLEX];		// Utilizado por el analizador lexico
int delantero=-1;		// Utilizado por el analizador lexico
int fin=0;				// Utilizado por el analizador lexico
int numLinea=1;			// Numero de Linea
entrada *tabla;				//declarar la tabla de simbolos
int tamTabla=TAMHASH;		//utilizado para cuando se debe hacer rehash
int elems=0;				//utilizado para cuando se debe hacer rehash

int cantidadError=0;	// Cantidad de errores

typedef int bool;		// Tipo de Datos booleanos
enum { false, true };	// Tipos booleanos
//FILE *salida;			// Salida del traductor


/************* Lexer ********************/
int h(const char* k, int m)
{
	unsigned h=0,g;
	int i;
	for (i=0;i<strlen(k);i++)
	{
		h=(h << 4) + k[i];
		if (g=h&0xf0000000){
			h=h^(g>>24);
			h=h^g;
		}
	}
	return h%m;
}
void insertar(entrada e);

void initTabla()
{	
	int i=0;
	
	tabla=(entrada*)malloc(tamTabla*sizeof(entrada));
	for(i=0;i<tamTabla;i++)
	{
		tabla[i].compLex=-1;
	}
}

int esprimo(int n)
{
	int i;
	for(i=3;i*i<=n;i+=2)
		if (n%i==0)
			return 0;
	return 1;
}

int siguiente_primo(int n)
{
	if (n%2==0)
		n++;
	for (;!esprimo(n);n+=2);

	return n;
}

//en caso de que la tabla llegue al limite, duplicar el tamaño
void rehash()
{
	entrada *vieja;
	int i;
	vieja=tabla;
	tamTabla=siguiente_primo(2*tamTabla);
	initTabla();
	for (i=0;i<tamTabla/2;i++)
	{
		if(vieja[i].compLex!=-1)
			insertar(vieja[i]);
	}		
	free(vieja);
}

//insertar una entrada en la tabla
void insertar(entrada e)
{
	int pos;
	if (++elems>=tamTabla/2)
		rehash();
	pos=h(e.lexema,tamTabla);
	while (tabla[pos].compLex!=-1)
	{
		pos++;
		if (pos==tamTabla)
			pos=0;
	}
	tabla[pos]=e;

}
//busca una clave en la tabla, si no existe devuelve NULL, posicion en caso contrario
entrada* buscar(const char *clave)
{
	int pos;
	entrada *e;
	pos=h(clave,tamTabla);
	while(tabla[pos].compLex!=-1 && strcmp(tabla[pos].lexema,clave)!=0 )
	{
		pos++;
		if (pos==tamTabla)
			pos=0;
	}
	return &tabla[pos];
}

void insertTablaSimbolos(const char *s, int n)
{
	entrada e;
	sprintf(e.lexema,s);
	e.compLex=n;
	insertar(e);
}

void initTablaSimbolos()
{
	int i;
	entrada pr,*e;
	
	insertTablaSimbolos(",",COMA);
	insertTablaSimbolos(":",DOS_PUNTOS);
	insertTablaSimbolos("[",L_CORCHETE);
	insertTablaSimbolos("]",R_CORCHETE);
	insertTablaSimbolos("{",L_LLAVE);
	insertTablaSimbolos("}",R_LLAVE);
	insertTablaSimbolos("true",PR_TRUE);
	insertTablaSimbolos("false",PR_FALSE);
	insertTablaSimbolos("TRUE",PR_TRUE);
	insertTablaSimbolos("FALSE",PR_FALSE);
	insertTablaSimbolos("NULL",PR_NULL);
	insertTablaSimbolos("null",PR_NULL);
	
}

void error(const char* mensaje)
{
	printf("\nLin %d: Error Lexico. %s.\n",numLinea,mensaje);	
}

void sigLex()
{
	int i=0, longid=0;
	char c=0;
	int acepto=0;
	int estado=0;
	char msg[41];
	entrada e;

	while((c=fgetc(archivo))!=EOF)
	{
		
		if (c==' ' || c=='\t')
			continue;	//eliminar espacios en blanco
		else if(c=='\n')
		{
			//incrementar el numero de linea
			numLinea++;
			continue;
		}
		else if (c=='"')
		{
			//elimina el comentario
			i=0;
			while(c!=EOF)
			{
				c=fgetc(archivo);
				id[i]=c;
				i++;
				if (c=='"'){
					id[i-1]='\0';
					break;
				}
				else if(c=='\n')
				{
					//incrementar el numero de linea
					numLinea++;
				}
			}
			if (c==EOF)
			error("Se llego al fin de archivo sin finalizar un comentario");
			sprintf(e.lexema,id);
			e.compLex=LITERAL_CADENA;
			insertar(e);
			t.pe=buscar(id);
			t.compLex=LITERAL_CADENA;
			
			break;
		}
		else if (isalpha(c))
		{
			
			i=0;
			do{
				id[i]=c;
				i++;
				c=fgetc(archivo);
				if (i>=TAMLEX)
					error("Longitud de Identificador excede tamaño de buffer");
			}while(isalpha(c));
			id[i]='\0';
			if (c!=EOF)
				ungetc(c,archivo);
			else
				c=0;
			t.pe=buscar(id);

			if (t.pe->compLex==-1)
			{
				printf(" No se esperaba '%s'",id);
				error("token no encontrado");
				continue;
			}
			t.compLex=t.pe->compLex;
			break;
		}
		
		else if (isdigit(c))
		{
				//es un numero
				i=0;
				estado=0;
				acepto=0;
				id[i]=c;
				
				while(!acepto)
				{
					switch(estado){
					case 0: //una secuencia netamente de digitos, puede ocurrir . o e
						c=fgetc(archivo);
						if (isdigit(c))
						{
							id[++i]=c;
							estado=0;
						}
						else if(c=='.'){
							id[++i]=c;
							estado=1;
						}
						else if(tolower(c)=='e'){
							id[++i]=c;
							estado=3;
						}
						else{
							estado=6;
						}
						break;
					
					case 1://un punto, debe seguir un digito (caso especial de array, puede venir otro punto)
						c=fgetc(archivo);						
						if (isdigit(c))
						{
							id[++i]=c;
							estado=2;
						}
						else if(c=='.')
						{
							i--;
							fseek(archivo,-1,SEEK_CUR);
							estado=6;
						}
						else{
							sprintf(msg,"No se esperaba '%c'",c);
							estado=-1;
						}
						break;
					case 2://la fraccion decimal, pueden seguir los digitos o e
						c=fgetc(archivo);
						if (isdigit(c))
						{
							id[++i]=c;
							estado=2;
						}
						else if(tolower(c)=='e')
						{
							id[++i]=c;
							estado=3;
						}
						else
							estado=6;
						break;
					case 3://una e, puede seguir +, - o una secuencia de digitos
						c=fgetc(archivo);
						if (c=='+' || c=='-')
						{
							id[++i]=c;
							estado=4;
						}
						else if(isdigit(c))
						{
							id[++i]=c;
							estado=5;
						}
						else{
							sprintf(msg,"No se esperaba '%c'",c);
							estado=-1;
						}
						break;
					case 4://necesariamente debe venir por lo menos un digito
						c=fgetc(archivo);
						if (isdigit(c))
						{
							id[++i]=c;
							estado=5;
						}
						else{
							sprintf(msg,"No se esperaba '%c'",c);
							estado=-1;
						}
						break;
					case 5://una secuencia de digitos correspondiente al exponente
						c=fgetc(archivo);
						if (isdigit(c))
						{
							id[++i]=c;
							estado=5;
						}
						else{
							estado=6;
						}break;
					case 6://estado de aceptacion, devolver el caracter correspondiente a otro componente lexico
						if (c!=EOF)
							ungetc(c,archivo);
						else
							c=0;
						id[++i]='\0';
						acepto=1;
						t.pe=buscar(id);
						if (t.pe->compLex==-1)
						{
							sprintf(e.lexema,id);
							e.compLex=LITERAL_NUM;
							insertar(e);
							t.pe=buscar(id);
						}
						t.compLex=LITERAL_NUM;
						break;
					case -1:
						if (c==EOF)
							error("No se esperaba el fin de archivo");
						else
							error(msg);
						exit(1);
					}
				}
			break;
		}		
		
		else if (c==':')
		{
				t.compLex=DOS_PUNTOS;
				t.pe=buscar(":");
				break;
			
		}
		else if (c==',')
		{
			t.compLex=COMA;
			t.pe=buscar(",");
			break;
		}
			
		else if (c=='[')
		{
			t.compLex=L_CORCHETE;
			t.pe=buscar("[");
			break;
		}
		else if (c==']')
		{
			t.compLex=R_CORCHETE;
			t.pe=buscar("]");
			break;
		}
		
		else if (c=='{')
		{
			t.compLex=L_LLAVE;
			t.pe=buscar("{");
			break;
		}
		else if (c=='}')
		{
			t.compLex=R_LLAVE;
			t.pe=buscar("}");
			break;
		}
		
		else if (c!=EOF)
		{
			sprintf(msg,"%c no esperado",c);
			error(msg);
		}
	}
	if (c==EOF)
	{
		t.compLex=EOF;
		sprintf(e.lexema,"EOF");
		t.pe=&e;
	}
	
}

/************* Paser / traductor ********************/

/*Prototipado*/
void attribute_value();
void element_list();
void e();
void match(int proximoToken);
void tagname();
void attributes_list();
void attributes();
void a();
void attribute();
void element();
void panicMode(int , int );
bool buscarSegundo(int , int );
void scan();


void scan(){
	sigLex();
}

/*
void tabulador(){
	int i=0;
	for(i=0;i<tabcounter;i++)
	fprintf(salida,"%s","\t");
}
*/


void attribute_name(){
	if(LITERAL_CADENA== t.compLex){
		//fprintf(salida," ");
		//fprintf(salida,"%s",t.pe->lexema);
		match(LITERAL_CADENA);		
	}else {
		cantidadError = cantidadError + 1;
		printf("Error Sintactico Linea: %d \n", numLinea );
		panicMode(7,t.compLex);
	}
}

void attribute_value(){
	if(LITERAL_CADENA== t.compLex){
		//fprintf(salida,"%c%s%c", '\"',t.pe->lexema, '\"');
		match(LITERAL_CADENA);
	}
	else if(LITERAL_NUM== t.compLex){
		//fprintf(salida,"%c%s%c", '\"',t.pe->lexema, '\"');
		match(LITERAL_NUM);
	}
	else if(PR_TRUE== t.compLex){
		//fprintf(salida,"%c%s%c", '\"',t.pe->lexema, '\"');
		match(PR_TRUE);
	}
	else if(PR_FALSE== t.compLex){
		//fprintf(salida,"%c%s%c", '\"',t.pe->lexema, '\"');
		match(PR_FALSE);
	}
	else if(PR_NULL== t.compLex){
		//fprintf(salida,"%c%s%c", '\"',t.pe->lexema, '\"');
		match(PR_NULL);
	}
	else {
		cantidadError = cantidadError + 1;
		printf("Error Sintactico Linea: %d \n", numLinea );
		panicMode(8,t.compLex);
	}	
}	


void element_list(){
	if(L_CORCHETE == t.compLex || LITERAL_CADENA == t.compLex){
		element();
		e();
	}
	else {
		cantidadError = cantidadError + 1;
		printf("Error Sintactico Linea: %d \n", numLinea );
		panicMode(9,t.compLex);
	}
}	

void e(){
	if (COMA== t.compLex){
		match(COMA);
		element();
		e();
	}
	else if (R_CORCHETE != t.compLex){
		cantidadError = cantidadError + 1;
		printf("Error Sintactico Linea: %d \n", numLinea );
		panicMode(10,t.compLex);
	}
}

void match(int proximoToken){
	if (t.compLex==proximoToken){
		sigLex();	
	}
	else{
		cantidadError = cantidadError + 1;
		printf("Cierre incompleto o se esperaba: %s \n",tokens[proximoToken]);
		printf(" Linea: %d \n", numLinea );	
}
	
}


void element()
{
	char lexema_tagName[TAMLEX];
	char lexema_elementList[TAMLEX];
	if (t.compLex == L_CORCHETE)
	{
		match(L_CORCHETE);
		strcpy(lexema_tagName,t.pe->lexema);
		//tabulador();
		//fprintf(salida,"<");
	tabcounter++;
		//fprintf(salida,"%s",lexema_tagName);

		tagname();
		if(COMA == t.compLex)
		{
			match(COMA);
			if (L_LLAVE==t.compLex)
			{
				attributes();
				//fprintf(salida,">\n");
				match(COMA);
				if(L_CORCHETE == t.compLex)
				{
					element_list();	
				}
				else if (LITERAL_CADENA == t.compLex)
				{
					strcpy(lexema_elementList,t.pe->lexema);
					//tabulador();
					//fprintf(salida,"%s%s",lexema_elementList,"\n");
					element_list();
				}
			
		}else{
			//fprintf(salida,">\n");
			if(L_CORCHETE == t.compLex)
			{
				element_list();
			}
			else if (LITERAL_CADENA == t.compLex)
			{
				element_list();
			}
		}
	}
		tabcounter--;
		//tabulador();
		//fprintf(salida,"</");
		
		//fprintf(salida,"%s",lexema_tagName);	
		//fprintf(salida,">\n");
		

		match(R_CORCHETE);
		
	}
	else if (LITERAL_CADENA== t.compLex){
		match(LITERAL_CADENA);
	}
	else{
		cantidadError = cantidadError + 1;
		printf("Error Sintactico Linea: %d \n", numLinea );
		panicMode(1,t.compLex);
	
	}
	
}

void tagname(){
		if (LITERAL_CADENA== t.compLex){
			match(LITERAL_CADENA);
		}
		else{
			cantidadError = cantidadError + 1;
			printf("Error Sintactico Linea: %d \n", numLinea );
			panicMode(2,t.compLex);
		}
}

void attributes(){
	if (L_LLAVE== t.compLex){
		match(L_LLAVE);
		if (LITERAL_CADENA== t.compLex){
			attributes_list();
		}
		match(R_LLAVE);
		
	}
	else{
		cantidadError = cantidadError + 1;
		printf("Error Sintactico Linea: %d \n", numLinea );
		panicMode(3,t.compLex);
	}
}

void attributes_list(){
	if(LITERAL_CADENA== t.compLex){
		attribute();
		a();
	}
	else{
		cantidadError = cantidadError + 1;
		printf("Error Sintactico Linea: %d \n", numLinea );
		panicMode(4,t.compLex);
	}
}

void a(){
	if(COMA== t.compLex){
		match(COMA);
		attribute();
		a();
	}
	else if (R_LLAVE != t.compLex){
		cantidadError = cantidadError + 1;
		printf("Error Sintactico Linea: %d \n", numLinea );
		panicMode(5,t.compLex);
	}	
}

void attribute(){
	if (LITERAL_CADENA== t.compLex){
		attribute_name();
		match(DOS_PUNTOS);
		//fprintf(salida,"=");
		attribute_value();
	}
	else {
		cantidadError = cantidadError + 1;
		printf("Error Sintactico Linea: %d \n", numLinea );
		panicMode(6,t.compLex);
	}
}



bool buscarPrimero(int produccion, int tokenActual){
	
	int i=0;
	for(i=0; i<5; i++){
		if(tokenActual==first[produccion][i]){
				return true;
		}
	}
	return false;
}

bool buscarSegundo(int produccion, int tokenActual){
	
	int i=0;
	for(i=0; i<3; i++){
		if(tokenActual==next[produccion][i]){
				return true;
		}
	}
	return false;
}	

void panicMode(int produccion, int tokenActual){
	
	bool existeSegundo = buscarSegundo(produccion,tokenActual);
	
	if(!existeSegundo){
		bool existePrimero = buscarPrimero(produccion,tokenActual);
		if (!existePrimero){
			scan();
		}
	}
}



int main(int argc,char* args[])
{
	initTabla();
	if(argc > 1)
	{
		if (!(archivo=fopen(args[1],"rt"))){
			printf("Archivo no encontrado.\n");
			exit(1);
		}
		//salida = fopen("output.xml","w");
		
		sigLex();		//inicia el lexer
		element();		//inica parser
		
		if(cantidadError>0){
			printf("ERROR\n");
		}else {
			printf("Sintaxis correcta\n");
		}
		fclose(archivo);
		//fclose(salida);	
	}else{
		printf("Debe pasar como parametro el path al archivo fuente.\n");
		exit(1);
	}
	return 0;	
}
