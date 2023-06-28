package preda_pec1;

// Documentacion utilizada: https://docs.oracle.com/javase/8/docs/api/java/util/Comparator.html
public class Objeto implements Comparable<Objeto>{
	
	int peso;
	float beneficio;
	float valorFracc;
	float valorFraccMoch;
	
	// CONSTRUCTOR
	public Objeto(int peso, float beneficio, float valorFraccMoch) {
		super();
		this.peso = peso;
		this.beneficio = beneficio;
		this.valorFracc = (float)peso/(float)beneficio;
		this.valorFraccMoch = valorFraccMoch;
	}

	// GETTERS y SETTERS utilizados
	public int getPeso() { return peso; }	
	public float getValorFracc() { return valorFracc; }
	public float getBeneficio() { return beneficio; }
	public float getValorFraccMoch() { return valorFraccMoch; }
	
	/*
	 * Compara objetos segun la fraccion
	 * @return 0 si el objeto parametrizado tiene el mismo valor que el actual
	 * @return 1 si el objeto parametrizado tiene un valor inferior al actual
	 * @return -1 si el objeto parametrizado tiene un valor superior al actual
	 */
	@Override
	public int compareTo(Objeto o) {
		if(o.valorFracc == this.valorFracc) {
			return 0;
		} else if (o.valorFracc < this.valorFracc) {
			return 1;
		} else {
			return -1;
		}
	}
}
