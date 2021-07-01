package org.skyve.metadata.controller;

import java.io.File;
import java.io.UnsupportedEncodingException;

import org.skyve.content.Disposition;
import org.skyve.content.MimeType;
import org.skyve.domain.messages.DomainException;
import org.skyve.util.Util;

/**
 * Represents a file stream to a client.
 * This is the result of executing the download method.
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
		try {
			this.bytes = content.getBytes(Util.UTF8);
		}
		catch (UnsupportedEncodingException e) {
			throw new DomainException("Cannot create download from String content", e);
		}
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
		try {
			this.bytes = content.getBytes(Util.UTF8);
		}
		catch (UnsupportedEncodingException e) {
			throw new DomainException("Cannot create download from String content", e);
		}
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
	 * 
	 * @return
	 */
	public String getFileName() {
		return fileName;
	}

	/**
	 * 
	 * @return
	 */
	public byte[] getBytes() {
		return bytes;
	}

	/**
	 * 
	 * @return
	 */
	public File getFile() {
		return file;
	}

	/**
	 * 
	 * @return
	 */
	public WebFileInputStream getInputStream() {
		return stream;
	}

	/**
	 * 
	 * @return
	 */
	public MimeType getMimeType() {
		return mimeType;
	}

	/**
	 * 
	 * @return
	 */
	public Disposition getDisposition() {
		return disposition;
	}
}
