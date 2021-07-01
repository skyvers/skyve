package org.skyve.metadata.controller;

import org.skyve.content.MimeType;

/**
 * Represents a file stream from a client.
 * This is an argument when executing a upload action.
 */
public class Upload {
	private String fileName;
	private WebFileInputStream stream;
	private MimeType mimeType;
	
	/**
	 * Constructor
	 * @param fileName
	 * @param stream
	 * @param mimeType
	 */
	public Upload(String fileName, WebFileInputStream stream, MimeType mimeType) {
		this.fileName = fileName;
		this.stream = stream;
		this.mimeType = mimeType;
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
}
