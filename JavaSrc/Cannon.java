import java.util.ArrayList;

public class Cannon extends Piece {
    public Cannon(int x, int y, boolean red){
        super(x, y, red);
    }

    @Override
    public Move[] getMoves(ChessBoard board){
        ArrayList<Move> mList = new ArrayList<Move>();
        for(int i = 1; getY() - i >= 0; i++){
            OMove up = new OMove(0, -i);
            if(up.canMove(this, board))
            mList.add(up);
        }
        for(int i = 1; getY() + i < ChessBoard.HEIGHT; i++){
            OMove down = new OMove(0, i);
            if(down.canMove(this, board))
            mList.add(down);
        }
        for(int i = 1; getX() - i >= 0; i++){
            OMove left = new OMove(-i, 0);
            if(left.canMove(this, board))
            mList.add(left);
        }
        for(int i = 1; getX() + i < ChessBoard.WIDTH; i++){
            OMove right = new OMove(i, 0);
            if(right.canMove(this, board))
            mList.add(right);
        }
        
        Move[] ms = new Move[mList.size()];
        mList.toArray(ms);
        return ms;
    }
    
}
