package modules.admin.Audit.job.support;

import static modules.admin.Audit.job.support.ArchiveUtils.ARCHIVE_CHARSET;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.channels.FileChannel;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;

public class BufferedLineReader implements AutoCloseable {

    private static final int BUFFER_CAPACITY = 0x4000;
    private static final int LF = '\n';

    private final ByteBuffer buffer = ByteBuffer.allocate(BUFFER_CAPACITY);

    /**
     * Container for lines emitted by BufferedLineReader, includes the start byte
     * offset of the line and the end byte offset.
     * 
     * @param line The String contents of the line
     * @param offset The byte offest where this line begins
     * @param end The byte offset where the <i>next</i> line begins
     */
    public static record Line(String line, long offset, long end) {

        /**
         * The length (in bytes) of this line; not including any line feed character
         * that may follow the line.
         * 
         * @return
         */
        public long length() {
            return this.end() - this.offset() - 1;
        }
    }

    /**
     * Keep track of which byte we're reading.
     */
    private long readPosition = 0;

    /**
     * The offset of the line we're currently reading, to be returned
     * along with the line content once we hit the end of line or
     * file.
     */
    private long lineOffset = 0;

    private final FileChannel fileChannel;

    public BufferedLineReader(Path path, long startOffseet) throws IOException {
        fileChannel = FileChannel.open(path, StandardOpenOption.READ);

        readPosition = lineOffset = fileChannel.position(startOffseet)
                                               .position();
    }

    /**
     * Read the next Line from this reader.
     * 
     * @return the next Line, or null if the end of the file was reached and no data has been read in
     * @throws IOException
     */
    public Line readLine() throws IOException {

        ByteArrayOutputStream baos = new ByteArrayOutputStream();

        while (true) {

            buffer.clear();
            int bytesRead = fileChannel.read(buffer);

            if (bytesRead <= 0) {
                break;
            }

            // Read one byte at time from the current buffer
            // looking for a line feed
            for (int i = 0; i < bytesRead; ++i) {
                byte byt = buffer.get(i);

                ++readPosition;
                if (byt == LF) {

                    // Set aside the current line details
                    Line line = createLine(baos);

                    // Shift all of our offsets ready
                    // for the next read operation
                    lineOffset = readPosition;
                    fileChannel.position(readPosition);

                    // This likely hits performance a bit, as we
                    // reset the file channel position back, and re-read
                    // some data on the next readLine() call.

                    return line;
                }

                baos.write(byt);
            }

        }

        // Made it to EOF

        if (baos.size() == 0) {
            // No values to emit
            return null;
        }

        // Emit last line
        return createLine(baos);
    }

    private Line createLine(ByteArrayOutputStream baos) {
        return new Line(baos.toString(ARCHIVE_CHARSET), lineOffset, readPosition);
    }

    @Override
    public void close() throws IOException {
        fileChannel.close();
    }
}
