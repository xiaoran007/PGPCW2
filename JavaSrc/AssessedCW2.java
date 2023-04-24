import java.io.*;

/**
 * A class that contains the main method.
 *
 * A Chinese Chess board consists of 9 vertical lines and 10 horizontal lines
 *
 * S/s: Soldier
 * C/c: Cannon
 * R/r: Chariot/Rooks
 * H/h: Horse
 * E/e: Elephant
 * A/a: Guards/Advisor
 * G/g: General
 */
public class AssessedCW2{
    // the main method
    public static void main(String[] args){
		char[][] board = readFromFile("Chess.txt");

		ChessBoard cb = new ChessBoard(board);
		cb.printBoard();
		// boolean flag = cb.checkMate();
		// System.out.println(flag);
	
    }

    // read the first line from a file
    private static char[][] readFromFile(String path){
	// 9 vertical lines and 10 horizontal lines
		char[][] output = new char[9][10];
	// exception handling here
		int y = 0;
		try(BufferedReader br = new BufferedReader(new FileReader(path))){
	    	while(y < 10){
				String line = br.readLine();
				int x = 0;
				while(x < 9){
		    		output[x][y] = line.charAt(x);
		    		x++;
				}
			y++;
	    	}
		}catch(IOException e){
	    	System.out.println("An IOException happens while reading from a file: " + path);
	    	return null;
		}
	return output;
    }
}
