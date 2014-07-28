package org.skyve.bizport;

import java.io.IOException;
import java.io.OutputStream;
import java.util.Set;

/**
 * Adapts an Excel workbook.
 * This class collects SheetData (adaption of Excel sheets).
 * The materialize() is used to populate data in the spreadsheet, for a newly created WorkbookData.
 * The write() will put the Excel format (xls or xlsx) onto the output stream given.
 */
public interface BizPortWorkbook {
	/**
	 * 
	 */
	public static enum BizPortFormat {
		xls, xlsx
	}

	/**
	 * 
	 * @param key
	 * @return
	 */
	public BizPortSheet getSheet(SheetKey key);
	
	/**
	 * 
	 * @param key
	 * @param sheet
	 */
	public void addSheet(SheetKey key, BizPortSheet sheet);
	
	/**
	 * 
	 * @param key
	 * @return
	 */
	public BizPortSheet removeSheet(SheetKey key);
	
	/**
	 * 
	 * @return
	 */
	public Set<SheetKey> getSheetKeys();
	
	/**
	 * 
	 */
	public void materialise();

	/**
	 * 
	 * @param out
	 * @throws IOException
	 */
	public void write(OutputStream out) throws IOException;
	
	/**
	 * 
	 * @return
	 */
	public BizPortFormat getFormat();
}
