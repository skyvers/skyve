package org.skyve.metadata.controller;

import org.skyve.bizport.BizPortWorkbook;
import org.skyve.domain.messages.UploadException;
import org.skyve.metadata.MetaData;

/**
 * 
 */
public abstract class BizImportAction implements MetaData {
	/**
	 * 
	 * @param bizPortable
	 * @param problems
	 * @throws Exception
	 */
	public abstract void bizImport(BizPortWorkbook bizPortable, UploadException problems) throws Exception;
}
