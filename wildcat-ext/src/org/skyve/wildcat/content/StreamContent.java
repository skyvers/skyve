package org.skyve.wildcat.content;

import java.io.BufferedInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Date;
import java.util.Stack;

import org.skyve.content.MimeType;

public final class StreamContent extends Content {
	private String uuid;
	private MimeType mimeType = MimeType.plain;
	private InputStream stream;
	private Date lastModified;
	private boolean versionable;
	private Stack<Date> versions;

	public StreamContent(String bizCustomer, 
							String bizModule, 
							String bizDocument, 
							String bizDataGroupId, 
							String bizUserId,
							String bizId, 
							String attributeName) {
		this.bizCustomer = bizCustomer;
		this.bizModule = bizModule;
		this.bizDocument = bizDocument;
		this.bizDataGroupId = bizDataGroupId;
		this.bizUserId = bizUserId;
		this.bizId = bizId;
		this.attributeName = attributeName;
	}

	StreamContent(String customerName, String path) {
		setPath(customerName, path);
	}

	public String getUuid() {
		return uuid;
	}

	public void setUuid(String uuid) {
		this.uuid = uuid;
	}

	public MimeType getMimeType() {
		return mimeType;
	}

	public void setMimeType(MimeType mimeType) {
		this.mimeType = mimeType;
	}

	public InputStream getStream() {
		return stream;
	}

	public void setStream(InputStream stream) {
		this.stream = stream;
	}
	
	public byte[] getBytes() throws IOException {
		try (BufferedInputStream bis = new BufferedInputStream(stream)) {
			try (ByteArrayOutputStream baos = new ByteArrayOutputStream()) {
				byte[] bytes = new byte[1024]; // 1K
				int bytesRead = 0;
				while ((bytesRead = bis.read(bytes)) > 0) {
					baos.write(bytes, 0, bytesRead);
				}
				return baos.toByteArray();
			}
		}
	}

	public Date getLastModified() {
		return lastModified;
	}

	public void setLastModified(Date lastModified) {
		this.lastModified = lastModified;
	}

	public boolean isVersionable() {
		return versionable;
	}

	public void setVersionable(boolean versionable) {
		this.versionable = versionable;
	}

	public Stack<Date> getVersions() {
		return versions;
	}

	void setVersions(Stack<Date> versions) {
		this.versions = versions;
	}
}
