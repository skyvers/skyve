package modules.admin.Tag.actions;

import java.util.List;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.UploadException;
import org.skyve.impl.bizport.AbstractDataFileLoader.LoaderActivityType;
import org.skyve.impl.bizport.DataFileField;
import org.skyve.impl.bizport.DataFileField.LoadAction;
import org.skyve.impl.bizport.POISheetLoader;
import org.skyve.metadata.controller.UploadAction;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;
import org.skyve.web.WebContext;

import modules.admin.domain.Tag;
import modules.admin.domain.Tag.FilterAction;
import modules.admin.domain.Tagged;

public class UploadTagCriteria extends UploadAction<Tag> {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = -8154709480999519405L;
	
	@Override
	public Tag upload(Tag tag,
						UploadedFile file, 
						UploadException exception,
						WebContext webContext)
	throws Exception {
		Persistence persistence = CORE.getPersistence();
		User user = persistence.getUser();
		Customer customer = user.getCustomer();

		Module modTag = customer.getModule(Tag.MODULE_NAME);
		Document docTagged = modTag.getDocument(customer, Tagged.DOCUMENT_NAME);
		
		POISheetLoader loader = new POISheetLoader( file.getInputStream(), 0, tag.getUploadModuleName(), tag.getUploadDocumentName(), exception);
		loader.setActivityType(LoaderActivityType.FIND);
		if(Boolean.TRUE.equals(tag.getFileHasHeaders())){
			//skip headers
			loader.setDataIndex(1);
		}

		DataFileField searchField = new DataFileField(tag.getAttributeName(),0);
		switch(tag.getFilterOperator()){
		case equals:
			searchField.setLoadAction(LoadAction.LOOKUP_EQUALS);
			break;
		case like:
			searchField.setLoadAction(LoadAction.LOOKUP_LIKE);
			break;
		case contains:
			searchField.setLoadAction(LoadAction.LOOKUP_CONTAINS);
			break;
		default:
			break;
		}
		//set column index
		if(tag.getFilterColumn()!=null){
			searchField.setIndex(tag.getFilterColumn().intValue()-1);
		}
		loader.addField(searchField);
		
		List<Bean> beansToTag = loader.beanResults();
		for(Bean bean: beansToTag){
			
			if(FilterAction.tagRecordsThatMatch.equals(tag.getFilterAction())){
				//add bean to tagged
				Tagged tagged = Tagged.newInstance();
				tagged.setTag(tag);
				tagged.setTaggedModule(tag.getUploadModuleName());
				tagged.setTaggedDocument(tag.getUploadDocumentName());
				tagged.setTaggedBizId(bean.getBizId());
				
				persistence.upsertBeanTuple(tagged);
			} else if (FilterAction.unTagRecordsThatMatch.equals(tag.getFilterAction())){
				//remove the tagged record
				DocumentQuery qTagged = persistence.newDocumentQuery(Tagged.MODULE_NAME, Tagged.DOCUMENT_NAME);
				qTagged.getFilter().addEquals(Tagged.taggedModulePropertyName, tag.getUploadModuleName());
				qTagged.getFilter().addEquals(Tagged.taggedDocumentPropertyName, tag.getUploadDocumentName());
				qTagged.getFilter().addEquals(Tagged.taggedBizIdPropertyName, bean.getBizId());
				
				List<Tagged> taggeds = qTagged.beanResults();
				for(Tagged tagged: taggeds){
					persistence.delete(docTagged, tagged);
					Integer numberTagged = tag.getNumberTagged();
					tag.setNumberTagged((numberTagged == null) ? Integer.valueOf(0) : Integer.valueOf(numberTagged.intValue() - 1));
				}
			}			
		}
		tag.setNumberLoaded(new Integer(loader.getDataIndex()));
		tag.setNumberMatched(new Integer(beansToTag.size()));
		if(tag.getNumberTagged()!=null && tag.getNumberMatched()!=null){
			tag.setNumberTagged(new Integer(tag.getNumberTagged().intValue() + tag.getNumberMatched().intValue()));
		}

		return tag;
	}
}
