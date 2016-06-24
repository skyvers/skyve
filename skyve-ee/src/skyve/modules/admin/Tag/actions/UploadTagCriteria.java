package modules.admin.Tag.actions;

import java.util.List;

import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.ss.usermodel.WorkbookFactory;
import org.skyve.CORE;
import org.skyve.bizport.BizPortException;
import org.skyve.domain.Bean;
import org.skyve.domain.types.DateOnly;
import org.skyve.domain.types.DateTime;
import org.skyve.domain.types.Decimal10;
import org.skyve.domain.types.Decimal2;
import org.skyve.domain.types.Decimal5;
import org.skyve.impl.bizport.FileUploadUtil;
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

import modules.admin.domain.Tag;
import modules.admin.domain.Tag.FilterAction;
import modules.admin.domain.Tag.FilterOperator;
import modules.admin.domain.Tagged;

public class UploadTagCriteria extends UploadAction<Tag> {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = -8154709480999519405L;
	
	@Override
	public Tag upload(Tag tag,
						UploadedFile file, 
						BizPortException exeption,
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
			boolean treatEmptyNumericAsZero = false;
			try {
				if(AttributeType.memo.equals( attrFilter.getAttributeType())
						|| AttributeType.text.equals(attrFilter.getAttributeType())){
					operand = FileUploadUtil.getStringValueFromCell(row, col, true);
					isText = true;
				} else if (AttributeType.date.equals(attrFilter.getAttributeType())){
					operand= new DateOnly(FileUploadUtil.getDateValueFromCell(row, col));
				} else if (AttributeType.dateTime.equals(attrFilter.getAttributeType())){
					operand= new DateTime(FileUploadUtil.getDateValueFromCell(row, col));
				} else if (AttributeType.decimal10.equals(attrFilter.getAttributeType())){
					operand = new Decimal10(FileUploadUtil.getNumericValueFromCell(row, col, treatEmptyNumericAsZero).doubleValue());
				}else if (AttributeType.decimal2.equals(attrFilter.getAttributeType())){
					operand = new Decimal2(FileUploadUtil.getNumericValueFromCell(row, col, treatEmptyNumericAsZero).doubleValue());
				}else if (AttributeType.decimal5.equals(attrFilter.getAttributeType())){
					operand = new Decimal5(FileUploadUtil.getNumericValueFromCell(row, col, treatEmptyNumericAsZero).doubleValue());
				}else if ( AttributeType.longInteger.equals(attrFilter.getAttributeType())){
					operand = new Long(FileUploadUtil.getNumericValueFromCell(row, col, treatEmptyNumericAsZero).longValue());
				}else if (AttributeType.integer.equals(attrFilter.getAttributeType())){
					operand = new Integer(FileUploadUtil.getNumericValueFromCell(row, col, treatEmptyNumericAsZero).intValue());
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
}
