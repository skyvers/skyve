package org.skyve.metadata.controller;

import org.skyve.bizport.BizPortWorkbook;
import org.skyve.metadata.MetaData;
import org.skyve.web.WebContext;

/**
 * 
 */
public abstract class BizExportAction implements MetaData {
	/**
	 * 
	 * @param webContext
	 * @return
	 * @throws Exception
	 */
	public abstract BizPortWorkbook bizExport(WebContext webContext) throws Exception;
}
