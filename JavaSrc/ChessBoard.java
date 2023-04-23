import java.util.ArrayList;

/**
 * a class for chess board
 */

public class ChessBoard{

    // The width of a Chinese chess board
    public static final int WIDTH = 9;
    // The depth of a Chinese chess board
    public static final int HEIGHT = 10;
    
    // a 2D array for chess board
    char[][] board;

    // lists of chess pieces appear in the current board
    ArrayList<Piece> redPieces = new ArrayList<Piece>();
    ArrayList<Piece> blackPieces = new ArrayList<Piece>();

    // constructor
    public ChessBoard(char[][] b){
	this.board = b;
	findAllPieces();
    }


    // return true, if the given position is valid in the current board
    public boolean validPosition(int x, int y){
	if(x < 0 || x >= WIDTH || y < 0 || y >= HEIGHT)
	    return false;
	return true;
    }

    // print the board
    public void printBoard(){
	for(int i = 0; i < ChessBoard.HEIGHT; i++){
	    for(int j = 0; j < ChessBoard.WIDTH; j++){
		System.out.print(board[j][i]);
	    }
	    System.out.println();
	}
    }
    

    // check if a vertical line or a horizontal line are unoccupied
    public boolean unoccupied(int x1, int x2, int y1, int y2){
	int min, max;
	// vertical line
	if(x1 == x2){
	    min = max = y1;
	    if(y1 > y2)
		min = y2;
	    else
		max = y2;
	    // a for loop 
	    for(int i = min + 1; i < max; i++){
		// if any positions are occupied, return false
		if(getPieceAt(x1,i) != null)
		    return false;
	    }
	}
	// horizontal line
	if(y1 == y2){
	    min = max = x1;
	    if(x1 > x2)
		min = x2;
	    else
		max = x2;
	    // a for loop 
	    for(int i = min + 1; i < max; i++){
		// if any positions are occupied, return false
		if(getPieceAt(i,y1) != null)
		    return false;
	    }
	}
	return true;
    }

    // TODO: return a piece at the specified position
    public Piece getPieceAt(int x, int y){
	// this piece is initialised to null 
	Piece p = null;
	// ... 
	return p;
    }
    
    // TODO: find out all the pieces on the current chess board
    public void findAllPieces(){
	// ...
    }



    // TODO: return ture, if the black player was checked
    public boolean checkMate(){
	// ...
	return false;
    }

}
