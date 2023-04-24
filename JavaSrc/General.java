import java.util.ArrayList;

public class General extends Piece{
    public General(int x, int y, boolean red){
        super(x, y, red);
    }

    @Override
    public Move[] getMoves(ChessBoard board){
        ArrayList<Move> mList = new ArrayList<Move>();
        
        OMove up = new OMove(0, -1);
        if (up.canMove(this, board) && (new General(getX(), getY() - 1, isRed()).atPalace())){
            mList.add(up);
        }

        OMove down = new OMove(0, 1);
        if (down.canMove(this, board) && (new General(getX(), getY() + 1, isRed()).atPalace())){
            mList.add(down);
        }

        OMove left = new OMove(-1, 0);
        if (left.canMove(this, board) && (new General(getX() - 1, getY(), isRed()).atPalace())){
            mList.add(left);
        }

        OMove right = new OMove(1, 0);
        if (right.canMove(this, board) && (new General(getX() + 1, getY(), isRed()).atPalace())){
            mList.add(right);
        }


        Move[] ms = new Move[mList.size()];
	    mList.toArray(ms);
	    return ms;
    }    
}
