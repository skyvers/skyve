package org.skyve.metadata.controller;

import java.io.File;
import java.nio.charset.StandardCharsets;

import org.skyve.content.Disposition;
import org.skyve.content.MimeType;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Represents a file to be streamed to the browser as a download.
 *
 * <p>This is the result returned from
 * {@link DownloadAction#download(org.skyve.domain.Bean, org.skyve.web.WebContext)}
 * and carries the file name, MIME type, content disposition, and the content source
 * (byte array, {@link File}, or {@link WebFileInputStream}).
 *
 * <p>When a {@link WebFileInputStream} is used as the content source, Skyve will
 * not close the stream until its bytes have been written to the HTTP response.
 *
 * @see DownloadAction
 * @see WebFileInputStream
 */
public class Download {
	private final @Nonnull String fileName;
	@SuppressWarnings("resource")
	private final @Nullable WebFileInputStream stream;
	private final @Nullable File file;
	private final @Nullable byte[] bytes;
	private final @Nonnull MimeType mimeType;
	private final @Nullable Disposition disposition;

	/**
	 * Initialises a download with its metadata and content source.
	 *
	 * <p>Invariant: public constructors supply exactly one of {@code stream}, {@code file},
	 * or {@code bytes}; the other content sources are {@code null}.
	 *
	 * @param fileName the suggested browser file name
	 * @param stream the lifecycle-managed content stream, or {@code null} for another content source
	 * @param file the file to stream, or {@code null} for another content source
	 * @param bytes the buffered content, or {@code null} for another content source
	 * @param mimeType the content MIME type
	 * @param disposition the content disposition, or {@code null} to use the response writer's default
	 */
	private Download(@Nonnull String fileName,
						@Nullable WebFileInputStream stream,
						@Nullable File file,
						@Nullable byte[] bytes,
						@Nonnull MimeType mimeType,
						@Nullable Disposition disposition) {
		this.fileName = fileName;
		this.stream = stream;
		this.file = file;
		this.bytes = bytes;
		this.mimeType = mimeType;
		this.disposition = disposition;
	}

	/**
	 * Creates a download backed by the supplied bytes.
	 *
	 * @param fileName the suggested browser file name
	 * @param bytes the file content
	 * @param mimeType the content MIME type
	 * @param disposition the content disposition
	 */
	public Download(@Nonnull String fileName,
						@Nonnull byte[] bytes,
						@Nonnull MimeType mimeType,
						@Nonnull Disposition disposition) {
		this(fileName, null, null, bytes, mimeType, disposition);
	}

	/**
	 * Creates an attachment download backed by the supplied bytes.
	 *
	 * @param fileName the suggested browser file name
	 * @param bytes the file content
	 * @param mimeType the content MIME type
	 */
	public Download(@Nonnull String fileName, @Nonnull byte[] bytes, @Nonnull MimeType mimeType) {
		this(fileName, bytes, mimeType, Disposition.attachment);
	}

	/**
	 * Creates a UTF-8 encoded download backed by the supplied text.
	 *
	 * @param fileName the suggested browser file name
	 * @param content the text content to encode as UTF-8
	 * @param mimeType the content MIME type
	 * @param disposition the content disposition
	 */
	public Download(@Nonnull String fileName,
						@Nonnull String content,
						@Nonnull MimeType mimeType,
						@Nonnull Disposition disposition) {
		this(fileName, content.getBytes(StandardCharsets.UTF_8), mimeType, disposition);
	}

	/**
	 * Creates a UTF-8 encoded attachment download backed by the supplied text.
	 *
	 * @param fileName the suggested browser file name
	 * @param content the text content to encode as UTF-8
	 * @param mimeType the content MIME type
	 */
	public Download(@Nonnull String fileName, @Nonnull String content, @Nonnull MimeType mimeType) {
		this(fileName, content, mimeType, Disposition.attachment);
	}

	/**
	 * Creates a download backed by a file.
	 *
	 * @param fileName the suggested browser file name
	 * @param file the file to stream
	 * @param mimeType the content MIME type
	 * @param disposition the content disposition
	 */
	public Download(@Nonnull String fileName,
						@Nonnull File file,
						@Nonnull MimeType mimeType,
						@Nonnull Disposition disposition) {
		this(fileName, null, file, null, mimeType, disposition);
	}

	/**
	 * Creates an attachment download backed by a file.
	 *
	 * @param fileName the suggested browser file name
	 * @param file the file to stream
	 * @param mimeType the content MIME type
	 */
	public Download(@Nonnull String fileName, @Nonnull File file, @Nonnull MimeType mimeType) {
		this(fileName, file, mimeType, Disposition.attachment);
	}

	/**
	 * Creates a download backed by a lifecycle-managed stream.
	 *
	 * <p>The stream is not closed until download processing has completed.
	 *
	 * @param fileName the suggested browser file name
	 * @param stream the stream containing the file content
	 * @param mimeType the content MIME type
	 * @param disposition the content disposition
	 */
	public Download(@Nonnull String fileName,
						@Nonnull WebFileInputStream stream,
						@Nonnull MimeType mimeType,
						@Nonnull Disposition disposition) {
		this(fileName, stream, null, null, mimeType, disposition);
	}

	/**
	 * Creates an attachment download backed by a lifecycle-managed stream.
	 *
	 * <p>The stream is not closed until download processing has completed.
	 *
	 * @param fileName the suggested browser file name
	 * @param stream the stream containing the file content
	 * @param mimeType the content MIME type
	 */
	public Download(@Nonnull String fileName, @Nonnull WebFileInputStream stream, @Nonnull MimeType mimeType) {
		this(fileName, stream, mimeType, Disposition.attachment);
	}
	
	/**
	 * Returns the suggested file name for the browser download prompt.
	 *
	 * @return file name; never {@code null}
	 */
	public @Nonnull String getFileName() {
		return fileName;
	}

	/**
	 * Returns the raw bytes of the file content, if the download was constructed from a byte array or String.
	 *
	 * @return the content bytes, or {@code null} if the content source is a {@link File} or stream
	 */
	public @Nullable byte[] getBytes() {
		return bytes;
	}

	/**
	 * Returns the file on disk to stream, if the download was constructed from a {@link File}.
	 *
	 * @return the file, or {@code null} if the content source is bytes or a stream
	 */
	public @Nullable File getFile() {
		return file;
	}

	/**
	 * Returns the lifecycle-managed input stream, if the download was constructed from a stream.
	 *
	 * @return the stream, or {@code null} if the content source is bytes or a file
	 */
	public @Nullable WebFileInputStream getInputStream() {
		return stream;
	}

	/**
	 * Returns the MIME type to declare in the HTTP {@code Content-Type} header.
	 *
	 * @return the MIME type; never {@code null}
	 */
	public @Nonnull MimeType getMimeType() {
		return mimeType;
	}

	/**
	 * Returns the HTTP content disposition ({@code attachment} or {@code inline}).
	 *
	 * @return the disposition; defaults to {@link org.skyve.content.Disposition#attachment}
	 */
	public @Nullable Disposition getDisposition() {
		return disposition;
	}
}
