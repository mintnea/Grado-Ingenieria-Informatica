package preda_pec1;

import java.util.*;

public class AlgoritmoVoraz {
	
	/*
	 * Algoritmo voraz
	 * @params array de pesos obtenidos del fichero de entrada
	 * @params array de valores obtenidos del fichero de entrada
	 * @params M valor total de la mochila
	 * @return Mochila
	 */
	public static Mochila algoritmo(int _pesoMaxMochila, boolean _isTraza, Mochila _inputBag) {		
		if(_isTraza) {
			System.out.println("");
			System.out.println("// ALGORITMO VORAZ //");
		}
		
		//Mochila mochOriginal = new Mochila(p.length);
		Mochila outputBag = new Mochila(_inputBag.objetos.length);
		float[] x = new float[_inputBag.objetos.length];
		float peso = 0;

		// Documentacion utilizada: https://docs.oracle.com/javase/7/docs/api/java/util/Arrays.html
		if(_isTraza)
			System.out.println("	// Se ordenan los objetos segun su relacion peso/valor");
		Arrays.sort(_inputBag.objetos); // Metodo de ordenacion segun los valores del objeto O(n log n)
		
		while (peso < _pesoMaxMochila) {
			for (int i = 0; i < _inputBag.objetos.length; i++) {
				if(_isTraza)
					System.out.println("	// Objeto " + i + " con peso " + _inputBag.objetos[i].getPeso() + " y valor " + _inputBag.objetos[i].getBeneficio());
				if(peso + _inputBag.objetos[i].getPeso() <= _pesoMaxMochila) {
					x[i] = 1;
					peso = (float)peso + _inputBag.objetos[i].getPeso();
					outputBag.newObjeto(_inputBag.objetos[i]);
					if(_isTraza)
						System.out.println("		// No supera el total de la mochila, se mete entero con su valor entero.");
				} else if((float)(_pesoMaxMochila - peso)/_inputBag.objetos[i].getPeso() != 0){
					x[i] = (float)(_pesoMaxMochila - peso)/_inputBag.objetos[i].getPeso();
					peso = _pesoMaxMochila;
					float nuevoBeneficio = _inputBag.objetos[i].getBeneficio()*x[i];
					outputBag.newObjeto(new Objeto(_inputBag.objetos[i].getPeso(), nuevoBeneficio, x[i]));
					if(_isTraza)
						System.out.println("		// Al superar el peso, se fracciona su peso en un "+ x[i]*100 +"% quedando un nuevo valor de " + nuevoBeneficio);
				} else {
					break;
				}
			}
			
		}
		return outputBag;
	}
}
