package preda_pec1;

import java.io.*;
import java.util.Arrays;
import java.util.List;

public class Main {
	
	public static BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
	
	/*
	 * Comprueba la validez del fichero de entrada
	 * @param file
	 * @return boolean
	 */
	public static Boolean checkInput(String file) throws IOException {
		File f = new File(file);
		if(!f.exists() || !f.isFile() || !f.canRead()) {
			System.out.println("** INFO: No se ha proporcionado fichero de entrada.");
			System.out.println("** INFO: Se requeriran los datos durante la ejecucion.");
			return false;
		} else {
			try {
				BufferedReader br = new BufferedReader(new InputStreamReader(new FileInputStream(file), "utf-8"));
				int controllInput = Integer.parseInt(br.readLine());
			} catch (Exception e) {
				System.out.println("** INFO: No se ha proporcionado fichero de entrada.");
				System.out.println("** INFO: Se requeriran los datos durante la ejecucion.");
				return false;
			}
			return true;
		}
	}
	
	/*
	 * Comprueba la validez del fichero de salida
	 * @param file
	 * @return boolean
	 */
	public static Boolean checkOutput(String file) {
		if(!file.contains(".txt") && !file.contains(".dat")) {
			System.out.println("** INFO: Fichero de salida no proporcionado, el resultado se proporcionara por pantalla.");
			return false;
		}
		File f = new File(file);
		f=f.getAbsoluteFile();
		
		if(!f.getParentFile().exists() || !f.getParentFile().canWrite()) {
			System.out.println("** ERROR: No se puede manipular el fichero de salida.");
			return false;
		}
		if(f.exists()) {
			if(!f.isFile() || !f.canWrite()) {
				System.out.println("** ERROR: El archivo de salida no es un fichero o no puede sobreescribirse.");
				return false;
			}
		}
		return true;
	}
	
	/*
	 * Menu de ayuda
	 */
	public static void getHelp() {
		System.out.println("SINTAXIS: mochila-voraz [fichero entrada] [fichero salida] [-t][-h] ");
		System.out.println("-t\t\tTraza el algoritmo");
		System.out.println("-h\t\tMuestra esta ayuda");
		System.out.println("[fichero entrada]\t\tNombre del fichero de entrada");
		System.out.println("[fichero salida]\t\tNombre del fichero de salida");
	}
	
	/*
	 * Imprime los datos finales en el fichero parametrizado
	 * @params Mochila
	 * @params bw fichero sobre el que escribir
	 */
	public static void interpretaData(Mochila m, BufferedWriter bw) throws IOException {
		bw.write("**Peso de la mochila: " + m.getPesoMochila() + "\n");
		bw.write("** Beneficio total maximizado: " + m.getBeneficioTotal() + "\n");
		bw.write("==========================" + "\n");
		for (int b = 0; b < m.objetos.length; b++) {
			if(m.objetos[b] != null)
				bw.write("** [" + (b+1) +"] | Peso: " + m.getObjetos()[b].getPeso() + " | Fraccion en la mochila: " + m.getObjetos()[b].getValorFraccMoch() + " | Beneficio fraccionado: " + m.getObjetos()[b].getBeneficio() + "\n");
			else
				break;
		}
		bw.write("==========================");
	}
	
	/*
	 * Muestra los datos finales por consola
	 * @params Mochila
	 */
	public static void interpretaData(Mochila m) {
		System.out.println("** Peso de la mochila: " + m.getPesoMochila());
		System.out.println("** Beneficio total maximizado: " + m.getBeneficioTotal());
		System.out.println("==========================");
		for (int b = 0; b < m.objetos.length; b++) {
			if(m.objetos[b] != null)
				System.out.println("** [" + (b+1) +"] | Peso: " + m.getObjetos()[b].getPeso() + " | Fraccion en la mochila: " + m.getObjetos()[b].getValorFraccMoch() + " | Beneficio fraccionado: " + m.getObjetos()[b].getBeneficio());
			else
				break;
		}
		System.out.println("==========================");
	}
	
	/*
	 * Metodo principal que ejecuta el programa
	 * @param args argumentos de entrada que proporciona el usuario
	 */
	public static void main(String[] args) throws IOException, NumberFormatException {
		long startTime = System.currentTimeMillis();
		//////
		if(args.length != 0) {
			List<String> transformedData = Arrays.asList(args);
			Boolean needHelp = transformedData.contains("-h"); // Muestra la ayuda
			Boolean isTraza = transformedData.contains("-t"); // Muestra la traza de lo que el script hace
			BufferedWriter bw = null;
			BufferedReader br = null;
			//////
			
			if(isTraza) {
				System.out.println("");
				System.out.println("// MAIN //");
			}
			
			// MARK: GET HELP
			if(needHelp)
				getHelp();
			
			// MARK: PREPARA FICHEROS O SALIDAS
			if(isTraza)
				System.out.println("	// Leyendo datos de entrada...");
			Boolean hasInputFile = checkInput(args[0]);
			if(hasInputFile)
				br = new BufferedReader(new InputStreamReader(new FileInputStream(args[0]), "utf-8")); // Lectura del fichero de entrada
				
			if(isTraza)
				System.out.println("	// Preparando fichero de salida...");
			if((hasInputFile && args.length > 1 && checkOutput(args[1])) || (!hasInputFile && checkOutput(args[0]))) { // Si ha aportado fichero de salida, el resultado se imprimira en el mismo en caso de ser valido
				if(hasInputFile)
					bw = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(args[1]), "utf-8"));
				else 
					bw = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(args[0]), "utf-8"));
			}
			
			// MARK: OBTIENE OBJETOS
			int numObjetos = 1, pointer = 0;
			if(hasInputFile) {
				numObjetos = Integer.parseInt(br.readLine()); // La primera linea siempre sera el total de objetos a leer
			} else {
				do {
					System.out.println("** Introduce numero (> 0) de objetos en la mochila:");
					numObjetos = Integer.parseInt(reader.readLine());
				} while (numObjetos < 0);
			}
			Mochila inputBag = new Mochila(numObjetos);
			if(isTraza)
				System.out.println("	// Se obtiene numero de objetos: " + numObjetos);
				
			
			// MARK: OBTIENE PESOS Y VALOR DE LOS OBJETOS
			while (pointer <  numObjetos) { // Se leen los datos de todos los objetos
				int peso = 0, valor = 0;
				if(hasInputFile) {
					String linea = br.readLine(); // lectura
					peso = Integer.parseInt(linea.split(" ")[0]); // primer dato = peso del objeto
					valor = Integer.parseInt(linea.split(" ")[1]); // segundo dato = valor del objeto
				} else {
					do {
						System.out.println("** Introduce peso (> 0) del objeto " + pointer + ":");
						peso = Integer.parseInt(reader.readLine());
					} while (peso < 0);
					do {
						System.out.println("** Introduce valor (> 0) del objeto " + pointer + ":");
						valor = Integer.parseInt(reader.readLine());
					} while (valor < 0);
				}
				
				inputBag.newObjeto(new Objeto(peso,valor,1));
				
				if(isTraza) {
					System.out.println("	// Objeto "  + pointer + " guardado en la mochila original:");
					System.out.println("		// Peso: "+ peso);
					System.out.println("		// Valor: "+ valor);
				}
				pointer ++;
			}
			
			// MARK: OBTIENE PESO MAXIMO MOCHILA
			int pesoMaxMochila = 1;
			if(hasInputFile) {
				pesoMaxMochila = Integer.parseInt(br.readLine()); // la ultima linea sera el peso total de la mochila
			} else {
				do {
					System.out.println("** Introduce el peso total (> 0) de la mochila:");
					pesoMaxMochila = Integer.parseInt(reader.readLine());
				} while (pesoMaxMochila < 0);
			}
			if(isTraza)
				System.out.println("	// Se obtiene el peso total: " + pesoMaxMochila);
			
			// MARK: CALCULO
			if(bw != null) { // en caso de haberse proporcionado fichero de salida se mostraran los datos a traves de el
				interpretaData(preda_pec1.AlgoritmoVoraz.algoritmo(pesoMaxMochila, isTraza, inputBag), bw);
				System.out.println("** Resultados en el archivo indicado.");
				bw.close();
			}else { // Sino se escribe por consola
				interpretaData(preda_pec1.AlgoritmoVoraz.algoritmo(pesoMaxMochila, isTraza, inputBag));
			}
			if(br!=null)
				br.close();
			long endTime = System.currentTimeMillis() - startTime;
			System.out.println("Tiempo de ejecucion: " + endTime + " ms");
		}
	}
}
