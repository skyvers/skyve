package modules.admin.Tag.actions;

import java.util.Date;
import java.util.List;

import modules.admin.domain.Tag;
import modules.admin.domain.Tag.FilterAction;
import modules.admin.domain.Tag.FilterOperator;
import modules.admin.domain.Tagged;

import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.ss.usermodel.WorkbookFactory;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.types.DateOnly;
import org.skyve.domain.types.DateTime;
import org.skyve.domain.types.Decimal10;
import org.skyve.domain.types.Decimal2;
import org.skyve.domain.types.Decimal5;
import org.skyve.metadata.controller.UploadAction;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;
import org.skyve.web.WebContext;

public class UploadTagCriteria extends UploadAction<Tag> {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = -8154709480999519405L;
	
	@Override
	public Tag upload(Tag tag,
						UploadedFile file,
						WebContext webContext)
	throws Exception {
		Persistence persistence = CORE.getPersistence();
		User user = persistence.getUser();
		Customer customer = user.getCustomer();
		Module modFilter = customer.getModule(tag.getUploadModuleName());
		Document docFilter = modFilter.getDocument(customer, tag.getUploadDocumentName());
		Attribute attrFilter = docFilter.getAttribute(tag.getAttributeName());

		Module modTag = customer.getModule(Tag.MODULE_NAME);
		Document docTagged = modTag.getDocument(customer, Tagged.DOCUMENT_NAME);
		
		Workbook workbook = WorkbookFactory.create(file.getInputStream());
		Sheet sheet = workbook.getSheetAt(0);
		
		int rowIndex = 0;
		if(Boolean.TRUE.equals(tag.getFileHasHeaders())){
			rowIndex = 1;
		}
		int col = 0;
		if(tag.getFilterColumn()!=null){
			col = tag.getFilterColumn().intValue()-1;
		}
		
		// TODO abuse of construct
		int numberLoaded = 0;
		while (true) {

			Row row = sheet.getRow(rowIndex);
			if (row == null) {
				break;
			}
			
			DocumentQuery q = persistence.newDocumentQuery(tag.getUploadModuleName(), tag.getUploadDocumentName());
			
			// general try - if value is not of the expected type or empty, throw an exception
			Object operand = null;
			boolean isText = false;
			try {
				if(AttributeType.memo.equals( attrFilter.getAttributeType())
						|| AttributeType.text.equals(attrFilter.getAttributeType())){
					operand = getStringValueFromCell(row, col, true);
					isText = true;
				} else if (AttributeType.date.equals(attrFilter.getAttributeType())){
					operand= new DateOnly(getDateValueFromCell(row, col));
				} else if (AttributeType.dateTime.equals(attrFilter.getAttributeType())){
					operand= new DateTime(getDateValueFromCell(row, col));
				} else if (AttributeType.decimal10.equals(attrFilter.getAttributeType())){
					operand = new Decimal10(getNumericValueFromCell(row, col).doubleValue());
				}else if (AttributeType.decimal2.equals(attrFilter.getAttributeType())){
					operand = new Decimal2(getNumericValueFromCell(row, col).doubleValue());
				}else if (AttributeType.decimal5.equals(attrFilter.getAttributeType())){
					operand = new Decimal5(getNumericValueFromCell(row, col).doubleValue());
				}else if ( AttributeType.longInteger.equals(attrFilter.getAttributeType())){
					operand = new Long(getNumericValueFromCell(row, col).longValue());
				}else if (AttributeType.integer.equals(attrFilter.getAttributeType())){
					operand = new Integer(getNumericValueFromCell(row, col).intValue());
				}
			} catch (Exception e){
				throw new Exception("Problem loading values at row " + rowIndex+1 + " column " + col+1 + ". The value may be the wrong type or invalid.");
			}				

			
			//inc number loaded
			numberLoaded++;
			
			if(FilterOperator.equals.equals(tag.getFilterOperator())){
				q.getFilter().addEquals(tag.getAttributeName(), operand);
			} else if (isText && FilterOperator.like.equals(tag.getFilterOperator())){
				q.getFilter().addLike(tag.getAttributeName(), (String) operand);
			} else if (isText && FilterOperator.contains.equals(tag.getFilterOperator())){
				StringBuilder sMatch = new StringBuilder();
				sMatch.append("%").append((String) operand).append("%");
				q.getFilter().addLike(tag.getAttributeName(), sMatch.toString());
			} else {
				throw new Exception("The filter operator you selected is not valid for the selected attribute.");
			}
			
			//add values to tagged			
			List<Bean> matches = q.projectedResults();
			for(Bean b:matches){
				Integer numberMatched = tag.getNumberMatched();
				tag.setNumberMatched((numberMatched == null) ? Integer.valueOf(1) : Integer.valueOf(numberMatched.intValue() + 1));
				
				if(FilterAction.tagRecordsThatMatch.equals(tag.getFilterAction())){
					//add bean to tagged
					Tagged tagged = Tagged.newInstance();
					tagged.setTag(tag);
					tagged.setTaggedModule(tag.getUploadModuleName());
					tagged.setTaggedDocument(tag.getUploadDocumentName());
					tagged.setTaggedBizId(b.getBizId());
					
					persistence.upsertBeanTuple(tagged);
				} else if (FilterAction.unTagRecordsThatMatch.equals(tag.getFilterAction())){
					//remove the tagged record
					DocumentQuery qTagged = persistence.newDocumentQuery(Tagged.MODULE_NAME, Tagged.DOCUMENT_NAME);
					qTagged.getFilter().addEquals(Tagged.taggedModulePropertyName, tag.getUploadModuleName());
					qTagged.getFilter().addEquals(Tagged.taggedDocumentPropertyName, tag.getUploadDocumentName());
					qTagged.getFilter().addEquals(Tagged.taggedBizIdPropertyName, b.getBizId());
					
					List<Tagged> taggeds = qTagged.beanResults();
					for(Tagged tagged: taggeds){
						persistence.delete(docTagged, tagged);
						Integer numberTagged = tag.getNumberTagged();
						tag.setNumberTagged((numberTagged == null) ? Integer.valueOf(0) : Integer.valueOf(numberTagged.intValue() - 1));
					}
				}
			}

			// move down to next row in spreadsheet
			rowIndex++;
		}
		tag.setNumberLoaded(new Integer(numberLoaded));

		return tag;
	}
	

	/**
	 * Wrapper to get a numeric value from the spreadsheet cell.
	 * 
	 * @param row
	 * @param col
	 * @return the numeric value
	 */
	// TODO is there a use in returning 0 if there is no value?
	private static Double getNumericValueFromCell (Row row, int col) throws Exception {
		Double result = Double.valueOf(0);


		Cell cell = row.getCell(col, Row.RETURN_BLANK_AS_NULL);
		if (cell != null) {
			result = Double.valueOf(cell.getNumericCellValue());
		} else {
			throw new Exception ("The cell is empty or not a valid number.");
		}

		return result;
	}

	/**
	 * Wrapper to get a string value from the spreadsheet cell.
	 * 
	 * @param row
	 * @param col
	 * @return The String Value of the cell
	 */
	// TODO is there any use in return "" if the cell is empty?
	private static String getStringValueFromCell (Row row, int col, boolean blankAsNull)throws Exception {
		String result = null;

		Cell cell = row.getCell(col, Row.RETURN_BLANK_AS_NULL);
		if (cell != null) {
			result = cell.getStringCellValue().trim();
		}
		if (result == null) {
			if (!blankAsNull) {
				result = "";
			}
		}

		return result;
	}


	/**
	 * Wrapper to get a date value from the spreadsheet cell.
	 * 
	 * @param row
	 * @param col
	 * @return The Date Value of the cell
	 */
	private static Date getDateValueFromCell (Row row, int col) throws Exception{
		Date result = null;

		Cell cell = row.getCell(col, Row.RETURN_BLANK_AS_NULL);
		if (cell != null) {
			result = cell.getDateCellValue();
		} else {
			throw new Exception ("The value in the cell is not a valid date.");
		}

		return result;
	}

}
