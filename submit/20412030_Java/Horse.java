import java.util.ArrayList;

public class Horse extends Piece{
    public Horse(int x, int y,boolean red){
        super(x, y, red);
    }

    @Override
    public Move[] getMoves(ChessBoard board){
        ArrayList<Move> mList = new ArrayList<Move>();
        // move up (up right or up left)
        HMove upLeft = new HMove(-1, -2);
        if (upLeft.canMove(this, board)){
            mList.add(upLeft);
        }
        HMove upRight = new HMove(1, -2);
        if (upRight.canMove(this, board)){
            mList.add(upRight);
        }
        HMove downLeft = new HMove(-1, 2);
        if (downLeft.canMove(this, board)){
            mList.add(downLeft);
        }
        HMove downRight = new HMove(1, 2);
        if (downRight.canMove(this, board)){
            mList.add(downRight);
        }
        
        Move[] ms = new Move[mList.size()];
        mList.toArray(ms);
        return ms;
    }
    
}
