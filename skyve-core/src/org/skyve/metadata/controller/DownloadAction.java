package org.skyve.metadata.controller;

import java.io.InputStream;

import org.skyve.content.Disposition;
import org.skyve.content.MimeType;
import org.skyve.domain.Bean;
import org.skyve.metadata.MetaData;
import org.skyve.web.WebContext;

/**
 * 
 * @param <T>
 */
public abstract class DownloadAction<T extends Bean> implements MetaData {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = -1360787878049667579L;

	/**
	 * 
	 */
	public static class Download {
		private String fileName;
		private InputStream is;
		private MimeType mimeType;
		private Disposition disposition;
		
		/**
		 * 
		 * @param fileName
		 * @param is
		 * @param mimeType
		 * @param disposition
		 */
		public Download(String fileName, InputStream is, MimeType mimeType, Disposition disposition) {
			this.fileName = fileName;
			this.is = is;
			this.mimeType = mimeType;
			this.disposition = disposition;
		}

		/**
		 * 
		 * @param fileName
		 * @param is
		 * @param mimeType
		 */
		public Download(String fileName, InputStream is, MimeType mimeType) {
			this(fileName, is, mimeType, Disposition.attachment);
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
		 * @param fileName
		 */
		public void setFileName(String fileName) {
			this.fileName = fileName;
		}
		
		/**
		 * 
		 * @return
		 */
		public InputStream getInputStream() {
			return is;
		}

		/**
		 * 
		 * @param is
		 */
		public void setInputStream(InputStream is) {
			this.is = is;
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
		 * @param mimeType
		 */
		public void setMimeType(MimeType mimeType) {
			this.mimeType = mimeType;
		}

		/**
		 * 
		 * @return
		 */
		public Disposition getDisposition() {
			return disposition;
		}

		/**
		 * 
		 * @param disposition
		 */
		public void setDisposition(Disposition disposition) {
			this.disposition = disposition;
		}
	}
	
	/**
	 * 
	 * @param bean	The bean to execute on.
	 * @param file	The file to process.
	 * @param webContext	The context to manipulate.
	 * @throws Exception
	 */
	public abstract Download download(T bean, WebContext webContext) throws Exception;
}
