import java.util.ArrayList;
/**
 * A java class for Chariot
 */
public class Chariot extends Piece{

    // constructor
    public Chariot(int x, int y, boolean red){
	super(x, y, red);
    }
    
    @Override
    public Move[] getMoves(ChessBoard board){
	ArrayList<Move> mList = new ArrayList<Move>();

	// move up
	for(int i = 1; getY() - i >= 0; i++){
	    OMove up = new OMove(0, -i);
	    // check if it is possible to move
	    if(up.canMove(this, board))
		mList.add(up);
	}
	// move down
	for(int i = 1; getY() + i < ChessBoard.HEIGHT; i++){
	    OMove down = new OMove(0, i);
	    // check if it is possible to move
	    if(down.canMove(this, board))
		mList.add(down);
	}
	// move left
	for(int i = 1; getX() - i >= 0; i++){
	    OMove left = new OMove(-i, 0);
	    // check if it is possible to move
	    if(left.canMove(this, board))
		mList.add(left);
	}
	// move right
	for(int i = 1; getX() + i < ChessBoard.WIDTH; i++){
	    OMove right = new OMove(i, 0);
	    // check if it is possible to move
	    if(right.canMove(this, board))
		mList.add(right);
	}
	
	Move[] ms = new Move[mList.size()];
	mList.toArray(ms);
	return ms;
    }
}
