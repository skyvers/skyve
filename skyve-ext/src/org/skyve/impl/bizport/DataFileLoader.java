package org.skyve.impl.bizport;

import java.util.Date;
import java.util.List;

import org.skyve.bizport.BizPortException;
import org.skyve.domain.Bean;

public interface DataFileLoader {
	
	public static enum LoaderActivityType {
		CREATE_ALL, CREATE_FIND, FIND
	}

	
	public void setCreateMissingAssocations(boolean create);
	public void setEmptyAsZero(boolean emptyAsZero);
	public void setException(BizPortException exception);
	
	/**
	 * Set the activity being performed as a result of the upload
	 * 
	 * @param activityType
	 */
	public void setActivityType(LoaderActivityType activityType);
	
	/**
	 * Set the module and document context for the loader
	 * @param moduleName
	 * @param documentName
	 * @throws Exception
	 */
	public void setDocumentContext(String moduleName, String documentName) throws Exception;
	
	/**
	 * Add a field definition to the loader
	 * 
	 * @param dff
	 * @throws Exception
	 */
	public void addField(DataFileField dff) throws Exception;
	
	/**
	 * Returns the current dataIndex
	 * 
	 * @return
	 */
	public int getDataIndex();

	/**
	 * Sets the current data to dataIndex
	 * @param rowIndex
	 * @throws Exception
	 */
	public void setDataIndex(int dataIndex) throws Exception;

	/**
	 * Loads data into a bean based on the field action definitions
	 * 
	 * @return
	 */
	public <T extends Bean> T  beanResult() throws Exception;

	/**
	 * Returns the beans created as a result of the load
	 * 
	 * @return
	 * @throws Exception
	 */
	public <T extends Bean> List<T>  beanResults() throws Exception;
	
	/**
	 * Returns the BizPortException which contains the warnings and exceptions encountered
	 * 
	 * @return
	 */
	public BizPortException getException();

	/**
	 * Returns a String value from the field
	 * 
	 * @param fieldIndex
	 * @param blankAsNull
	 * @return
	 * @throws Exception
	 */
	public String getStringFieldValue(int fieldIndex, boolean blankAsNull) throws Exception;
	
	/**
	 * Returns a Date value from the field
	 * @param fieldIndex
	 * @return
	 * @throws Exception
	 */
	public Date getDateFieldValue(int fieldIndex) throws Exception;
	
	
	/**
	 * Returns a numeric value from the field
	 * 
	 * @param fieldIndex
	 * @param emptyAsZero
	 * @return
	 * @throws Exception
	 */
	public Double getNumericFieldValue(int fieldIndex, boolean emptyAsZero) throws Exception;


	/**
	 * Moves to next row, line or section of data
	 * 
	 * @throws Exception
	 */
	public void nextData() throws Exception;
	
	/**
	 * Whether ther is more data to load
	 * @return
	 * @throws Exception
	 */
	public boolean hasNextData() throws Exception;
	
	/**
	 * Sets the index of the field to be examined
	 * 
	 * @param fieldIndex
	 */
	public void setFieldIndex(int fieldIndex);
	
	/**
	 * Returns true if there is nothing in this data
	 * 
	 * @return
	 * @throws Exception
	 */
	public boolean isNoData() throws Exception;
	
	/**
	 * Returns a description indicating the location of the value at fieldIndex
	 * @param fieldIndex
	 * @return
	 * @throws Exception
	 */
	public String getWhere(int fieldIndex) throws Exception;
	
	
	/**
	 * A string representation of the debug information for the data
	 * 
	 * @return
	 */
	public String debugData() throws Exception;
	
}
