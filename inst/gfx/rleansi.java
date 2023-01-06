import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;

public class rleansi {
  public static void main(String[] args) throws Exception {
    if (args.length!=2) {
      System.out.println("Format: java rleansi in.txt out.txt");
      System.exit(0);
    }
    int[] remember = new int[256];
    int remember_length = 0;
    int[] buffer = new int[256];
    int bindex = 0;
    DataInputStream dis  = new DataInputStream(new FileInputStream(new File(args[0])));
    DataOutputStream dos  = new DataOutputStream(new FileOutputStream(new File(args[1])));
    while (dis.available() > 0) {
      int b = dis.read();
      while ((b!=27) && (dis.available()>0)) {
        dos.write(b);
        b = dis.read();
      }
      if (dis.available() == 0) {
        dis.close();
        dos.close();
        System.exit(0);
      }
      bindex = 0;
      while (b!=109) {
        buffer[bindex++] = b;
        b = dis.read();
      }
      buffer[bindex++] = 109;
      
      boolean seen_before = true;
      if (remember_length != bindex) {
        seen_before = false;
      } else {
        for (int j=0; j<remember_length; j++) {
          if (remember[j] != buffer[j]) {
            seen_before = false;
            break;
          }
        }
      }
      if (!seen_before) {
        for (int i=0; i<bindex; i++) {
          dos.write(buffer[i]);
          remember[i]=buffer[i];
        }
        remember_length = bindex;
      }
    }
    dis.close();
    dos.close();
  }
}
