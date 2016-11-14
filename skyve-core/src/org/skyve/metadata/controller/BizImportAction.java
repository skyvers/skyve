package org.skyve.metadata.controller;

import org.skyve.bizport.BizPortWorkbook;
import org.skyve.domain.messages.UploadException;
import org.skyve.metadata.MetaData;

/**
 * 
 */
public abstract class BizImportAction implements MetaData {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = -3956169127991820538L;

	/**
	 * 
	 * @param bizPortable
	 * @param problems
	 * @throws Exception
	 */
	public abstract void bizImport(BizPortWorkbook bizPortable, UploadException problems) throws Exception;
}
