package preda_pec1;

public class Mochila{
	
	float pesoMochila;
	Objeto[] objetos;
	float beneficioTotal;
	
	int peso;
	int beneficio;
	float fraccion;
	
	// CONSTRUCTOR
	public Mochila(int totalObjetos) {
		super();
		this.pesoMochila = 0;
		this.objetos = new Objeto[totalObjetos]; // Contiene objetos en forma de array
		this.beneficioTotal = 0;
	}
	
	// GETTERS y SETTERS necesarios para el propio objeto mochila
	public float getPesoMochila() {
		return pesoMochila;
	}
	public void setPesoMochila(float pesoMochila) {
		this.pesoMochila = pesoMochila;
	}
	public Objeto[] getObjetos() {
		return objetos;
	}
	public float getBeneficioTotal() {
		return beneficioTotal;
	}
	public void setBeneficioTotal(float beneficioTotal) {
		this.beneficioTotal = beneficioTotal;
	}
	
	// SETTERS necesarios para el objeto alojado en la mochila
	public void setPeso(int peso) {
		this.peso = peso;
	}
	public void setFraccion(float fraccion) {
		this.fraccion = fraccion;
	}
	
	/*
	 * Creacion de un nuevo objeto tipo Objeto que se guardara en el objeto Mochila
	 * @params Objeto
	 */
	public void newObjeto(Objeto o) {
		for(int i = 0; i < this.objetos.length ; i ++) {
			if(this.objetos[i] == null) { // Escribe en una posicion no ocupada
				this.objetos[i] = o; // Se guarda el objeto parametrizado
				this.beneficioTotal += o.getBeneficio(); // Añade valor a la mochila
				this.peso += o.getPeso(); // Establece el peso del objeto
				this.pesoMochila += o.getPeso() * o.getValorFraccMoch(); // Añade peso al objeto Mochila (peso del objeto * valor fraccionado del objeto)
				break;
			}
		}
	}
	
	/*public static void quicksort(Objeto obj[], int izq, int der) {
		  Objeto pivote=obj[izq]; // se obtiene el pivote
		  int i=izq;         // izquierda a derecha
		  int j=der;         // derecha a izquierda
		  Objeto aux;

		  while(i < j){                          // mientras no se crucen las búsquedas                                   
		     while(obj[i].getBeneficio() <= pivote.getBeneficio() && i < j) i++; // busca elemento mayor que pivote
		     while(obj[j].getBeneficio() > pivote.getBeneficio()) j--;           // busca elemento menor que pivote
		     if (i < j) {                        
		         aux= obj[i];                      // los intercambia
		         obj[i]=obj[j];
		         obj[j]=aux;
		     }
		   }
		   
		   obj[izq]=obj[j];                                      
		   obj[j]=pivote;      // los menores a la izquierda / mayores a la derecha
		   
		   if(izq < j-1)
		      quicksort(obj,izq,j-1);          
		   if(j+1 < der)
		      quicksort(obj,j+1,der);          
		   
		}*/
}
