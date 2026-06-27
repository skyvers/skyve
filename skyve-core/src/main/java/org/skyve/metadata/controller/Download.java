package org.skyve.metadata.controller;

import java.io.File;
import java.nio.charset.StandardCharsets;

import org.skyve.content.Disposition;
import org.skyve.content.MimeType;

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
	private String fileName;
	private WebFileInputStream stream;
	private File file;
	private byte[] bytes;
	private MimeType mimeType;
	private Disposition disposition;
	
	private Download(String fileName, MimeType mimeType, Disposition disposition) {
		this.fileName = fileName;
		this.mimeType = mimeType;
		this.disposition = disposition;
	}

	/**
	 * byte[] constructor
	 * 
	 * @param fileName
	 * @param bytes
	 * @param mimeType
	 * @param disposition
	 */
	public Download(String fileName, byte[] bytes, MimeType mimeType, Disposition disposition) {
		this(fileName, mimeType, disposition);
		this.bytes = bytes;
	}

	/**
	 * byte[] constructor
	 * 
	 * @param fileName
	 * @param bytes
	 * @param mimeType
	 */
	public Download(String fileName, byte[] bytes, MimeType mimeType) {
		this(fileName, mimeType, Disposition.attachment);
		this.bytes = bytes;
	}

	/**
	 * String constructor
	 * 
	 * @param fileName
	 * @param bytes
	 * @param mimeType
	 * @param disposition
	 */
	public Download(String fileName, String content, MimeType mimeType, Disposition disposition) {
		this(fileName, mimeType, disposition);
		this.bytes = content.getBytes(StandardCharsets.UTF_8);
	}

	/**
	 * byte[] constructor
	 * 
	 * @param fileName
	 * @param bytes
	 * @param mimeType
	 */
	public Download(String fileName, String content, MimeType mimeType) {
		this(fileName, mimeType, Disposition.attachment);
		this.bytes = content.getBytes(StandardCharsets.UTF_8);
	}

	/**
	 * File constructor
	 * 
	 * @param fileName
	 * @param file
	 * @param mimeType
	 * @param disposition
	 */
	public Download(String fileName, File file, MimeType mimeType, Disposition disposition) {
		this(fileName, mimeType, disposition);
		this.file = file;
	}

	/**
	 * File constructor
	 * 
	 * @param fileName
	 * @param file
	 * @param mimeType
	 */
	public Download(String fileName, File file, MimeType mimeType) {
		this(fileName, mimeType, Disposition.attachment);
		this.file = file;
	}

	/**
	 * Stream constructor (note that DownloadInputStream will not be closed until download processing has occurred)
	 * 
	 * @param fileName
	 * @param stream
	 * @param mimeType
	 * @param disposition
	 */
	public Download(String fileName, WebFileInputStream stream, MimeType mimeType, Disposition disposition) {
		this(fileName, mimeType, disposition);
		this.stream = stream;
	}

	/**
	 * Stream constructor (note that DownloadInputStream will not be closed until download processing has occurred)
	 * 
	 * @param fileName
	 * @param is
	 * @param mimeType
	 */
	public Download(String fileName, WebFileInputStream stream, MimeType mimeType) {
		this(fileName, mimeType, Disposition.attachment);
		this.stream = stream;
	}
	
	/**
	 * Returns the suggested file name for the browser download prompt.
	 *
	 * @return file name; never {@code null}
	 */
	public String getFileName() {
		return fileName;
	}

	/**
	 * Returns the raw bytes of the file content, if the download was constructed from a byte array or String.
	 *
	 * @return the content bytes, or {@code null} if the content source is a {@link File} or stream
	 */
	public byte[] getBytes() {
		return bytes;
	}

	/**
	 * Returns the file on disk to stream, if the download was constructed from a {@link File}.
	 *
	 * @return the file, or {@code null} if the content source is bytes or a stream
	 */
	public File getFile() {
		return file;
	}

	/**
	 * Returns the lifecycle-managed input stream, if the download was constructed from a stream.
	 *
	 * @return the stream, or {@code null} if the content source is bytes or a file
	 */
	public WebFileInputStream getInputStream() {
		return stream;
	}

	/**
	 * Returns the MIME type to declare in the HTTP {@code Content-Type} header.
	 *
	 * @return the MIME type; never {@code null}
	 */
	public MimeType getMimeType() {
		return mimeType;
	}

	/**
	 * Returns the HTTP content disposition ({@code attachment} or {@code inline}).
	 *
	 * @return the disposition; defaults to {@link org.skyve.content.Disposition#attachment}
	 */
	public Disposition getDisposition() {
		return disposition;
	}
}
