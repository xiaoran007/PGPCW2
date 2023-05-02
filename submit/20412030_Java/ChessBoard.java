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

    // return a piece at the specified position
    public Piece getPieceAt(int x, int y){
	// this piece is initialised to null 
		Piece p = null;
		switch (board[x][y]) {
			case '.':
				break;
			case 'g':
				p = new General(x, y, false);
				break;
			case 'a':
				p = new Guard(x, y, false);
				break;
			case 'e':
				p = new Elephant(x, y, false);
				break;
			case 'h':
				p = new Horse(x, y, false);
				break;
			case 'r':
				p = new Chariot(x, y, false);
				break;
			case 'c':
				p = new Cannon(x, y, false);
				break;
			case 's':
				p = new Soldier(x, y, false);
				break;
			case 'G':
				p = new General(x, y, true);
				break;
			case 'A':
				p = new Guard(x, y, true);
				break;
			case 'E':
				p = new Elephant(x, y, true);
				break;
			case 'H':
				p = new Horse(x, y, true);
				break;
			case 'R':
				p = new Chariot(x, y, true);
				break;
			case 'C':
				p = new Cannon(x, y, true);
				break;
			case 'S':
				p = new Soldier(x, y, true);
				break;
			default:
				break;
		}
		return p;
    }
    
    // find out all the pieces on the current chess board
    public void findAllPieces(){
		for(int i = 0; i < ChessBoard.HEIGHT; i++){
			for(int j = 0; j < ChessBoard.WIDTH; j++){
				Piece p = null;
				p = getPieceAt(j, i);
				if (p == null){
					continue;
				}
				else if (p.isRed()){
					redPieces.add(p);
				}
				else if (!p.isRed()){
					blackPieces.add(p);
				}
				else {
					continue;
				}
			}
		}
    }



    // return ture, if the black player was checked
    public boolean checkMate(){
		int general_x = -1;
		int general_y = -1;
		Piece general = null;
		for (int i=0; i<blackPieces.size(); i++){
			Piece p = blackPieces.get(i);
			if (p.getClass().equals(General.class)){
				general = blackPieces.get(i);
				general_x = general.getX();
				general_y = general.getY();
				break;
			}
		}
		for (int i=0; i<redPieces.size(); i++){
			Piece p = redPieces.get(i);
			if (p.getClass().equals(Chariot.class)){
				if (p.getX() == general_x){
					if (unoccupied(p.getX(), general_x, p.getY(), general_y)){
						return true;
					}
				}
				else if (p.getY() == general_y){
					if (unoccupied(p.getX(), general_x, p.getY(), general_y)){
						return true;
					}
				}
			}
			else if (p.getClass().equals(Cannon.class)){
				if (p.getX() == general_x){
					int min_y = 0;
					int max_y = 0;
					if (general_y >= p.getY()){
						max_y = general_y;
						min_y = p.getY();
					}
					Piece mid = null;
					for (int j=min_y+1; j<max_y; j++){
						mid = getPieceAt(general_x, j);
						if (mid != null){
							if (unoccupied(general_x, mid.getX(), general_y, mid.getY()) && unoccupied(p.getX(), mid.getX(), p.getY(), mid.getY())){
								return true;
							}
						}
					}
				}
				else if (p.getY() == general_y){
					int min_x = 0;
					int max_x = 0;
					if (general_x >= p.getX()){
						max_x = general_x;
						min_x = p.getX();
					}
					Piece mid = null;
					for (int j=min_x+1; j<max_x; j++){
						mid = getPieceAt(j, general_y);
						if (mid != null){
							if (unoccupied(general_x, mid.getX(), general_x, mid.getY()) && unoccupied(p.getX(), mid.getX(), p.getY(), mid.getY())){
								return true;
							}
						}
					}
				}
			}
			else if (p.getClass().equals(Horse.class)){
				int h_x = p.getX();
				int h_y = p.getY();
				if (h_x+1 == general_x && h_y-2 == general_y){
					HMove move = new HMove(1, -2);
					if (move.canMove(p, this)){
						return true;
					}
				}
				else if (h_x-1 == general_x && h_y-2 == general_y){
					HMove move = new HMove(-1, -2);
					if (move.canMove(p, this)){
						return true;
					}
				}
				else if (h_x+1 == general_x && h_y+2 == general_y){
					HMove move = new HMove(1, 2);
					if (move.canMove(p, this)){
						return true;
					}
				}
				else if (h_x-1 == general_x && h_y+2 == general_y){
					HMove move = new HMove(-1, 2);
					if (move.canMove(p, this)){
						return true;
					}
				}
				else if (h_x-2 == general_x && h_y-1 == general_y){
					HMove move = new HMove(-2, -1);
					if (move.canMove(p, this)){
						return true;
					}
				}
				else if (h_x-2 == general_x && h_y+1 == general_y){
					HMove move = new HMove(-2, 1);
					if (move.canMove(p, this)){
						return true;
					}
				}
				else if (h_x+2 == general_x && h_y-1 == general_y){
					HMove move = new HMove(2, -1);
					if (move.canMove(p, this)){
						return true;
					}
				}
				else if (h_x+2 == general_x && h_y+1 == general_y){
					HMove move = new HMove(2, 1);
					if (move.canMove(p, this)){
						return true;
					}
				}
			}
		}
	return false;
    }

}
