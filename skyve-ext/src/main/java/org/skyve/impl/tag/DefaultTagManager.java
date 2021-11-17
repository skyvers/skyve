package org.skyve.impl.tag;

import java.util.ArrayList;
import java.util.List;

import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.app.admin.Tag;
import org.skyve.domain.app.admin.Tagged;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.persistence.AutoClosingIterable;
import org.skyve.persistence.BizQL;
import org.skyve.persistence.DocumentFilter;
import org.skyve.persistence.DocumentQuery;
import org.skyve.tag.TagManager;

public class DefaultTagManager implements TagManager {
	private static final DefaultTagManager INSTANCE = new DefaultTagManager();
	
	public static DefaultTagManager get() {
		return INSTANCE;
	}
	
	/**
	 * Disallow external instantiation
	 */
	private DefaultTagManager() {
		// nothing to see here
	}
	
	@Override
	public void tag(String tagId, Bean bean) throws Exception {
		tag(tagId, bean.getBizModule(), bean.getBizDocument(), bean.getBizId());
	}

	@Override
	public void tag(String tagId, String taggedModuleName, String taggedDocumentName, String taggedBizId) throws Exception {
		AbstractPersistence persistence = AbstractPersistence.get();
		User user = persistence.getUser();
		Customer customer = user.getCustomer();
		Module adminModule = customer.getModule(Tag.MODULE_NAME);
		Document tagDocument = adminModule.getDocument(customer, Tag.DOCUMENT_NAME);
		Document taggedDocument = adminModule.getDocument(customer, Tagged.DOCUMENT_NAME);
		PersistentBean tag = persistence.retrieve(tagDocument, tagId);

		PersistentBean tagged = taggedDocument.newInstance(user);
		BindUtil.set(tagged, Tagged.tagPropertyName, tag);
		BindUtil.set(tagged, Tagged.taggedModulePropertyName, taggedModuleName);
		BindUtil.set(tagged, Tagged.taggedDocumentPropertyName, taggedDocumentName);
		BindUtil.set(tagged, Tagged.taggedBizIdPropertyName, taggedBizId);

		try {
			persistence.preMerge(taggedDocument, tagged);
			persistence.upsertBeanTuple(tagged);
			persistence.postMerge(taggedDocument, tagged);
		}
		catch (DomainException e) {
			// do nothing - its a duplicate
			StringBuilder sb = new StringBuilder(256);
			sb.append(taggedModuleName).append('.').append(taggedDocumentName);
			sb.append('.').append(taggedBizId).append(" - ").append(e.getLocalizedMessage());
			System.err.println(sb.toString());
		}
	}

	@Override
	public void untag(String tagId, Bean bean) throws Exception {
		untag(tagId, bean.getBizModule(), bean.getBizDocument(), bean.getBizId());
	}

	@Override
	public void untag(String tagId, String taggedModuleName, String taggedDocumentName, String taggedBizId) throws Exception {
		AbstractPersistence persistence = AbstractPersistence.get();
		User user = persistence.getUser();

		BizQL deleteStatement = persistence.newBizQL(String.format("delete from {%s.%s} as bean " +
																	"where bean.%s.%s = :%s " +
																	"and bean.%s = :%s " +
																	"and bean.%s = :%s " +
																	"and bean.%s = :%s " +
																	"and bean.%s = :%s",
																		Tagged.MODULE_NAME,
																		Tagged.DOCUMENT_NAME,
																		Tagged.tagPropertyName,
																		Bean.DOCUMENT_ID,
																		Bean.DOCUMENT_ID,
																		Bean.USER_ID,
																		Bean.USER_ID,
																		Tagged.taggedModulePropertyName,
																		Tagged.taggedModulePropertyName,
																		Tagged.taggedDocumentPropertyName,
																		Tagged.taggedDocumentPropertyName,
																		Tagged.taggedBizIdPropertyName,
																		Tagged.taggedBizIdPropertyName));
		deleteStatement.putParameter(Bean.DOCUMENT_ID, tagId);
		deleteStatement.putParameter(Bean.USER_ID, user.getId());
		deleteStatement.putParameter(Tagged.taggedModulePropertyName, taggedModuleName);
		deleteStatement.putParameter(Tagged.taggedDocumentPropertyName, taggedDocumentName);
		deleteStatement.putParameter(Tagged.taggedBizIdPropertyName, taggedBizId);

		deleteStatement.execute();
	}

	@Override
	public String create(String tagName, boolean visible) throws Exception {
	    AbstractPersistence persistence = AbstractPersistence.get();
        User user = persistence.getUser();
        Customer customer = user.getCustomer();
        Module adminModule = customer.getModule(Tag.MODULE_NAME);
        Document tagDocument = adminModule.getDocument(customer, Tag.DOCUMENT_NAME);
        
        Tag tag = tagDocument.newInstance(user);
        tag.setName(tagName);
        tag.setVisible(Boolean.valueOf(visible));
        tag = persistence.save(tagDocument, tag);
        
        return tag.getBizId();
	}

	@Override
	public String getTagId(String tagName) throws Exception {
	    AbstractPersistence persistence = AbstractPersistence.get();
        User user = persistence.getUser();

        DocumentQuery query = persistence.newDocumentQuery(Tag.MODULE_NAME, Tag.DOCUMENT_NAME);
        query.addBoundProjection(Bean.DOCUMENT_ID);
        DocumentFilter filter = query.getFilter();
        filter.addEquals(Tag.namePropertyName, tagName);
        filter.addEquals(Bean.USER_ID, user.getId());

        List<Bean> results = query.projectedResults();
        String result = null;
        if (! results.isEmpty()) {
        	result = (String) BindUtil.get(results.get(0), Bean.DOCUMENT_ID);
        }
        
        return result;
	}

	@Override
	public List<DomainValue> getTags() throws Exception {
	    AbstractPersistence persistence = AbstractPersistence.get();
        User user = persistence.getUser();
        
        DocumentQuery query = persistence.newDocumentQuery(Tag.MODULE_NAME, Tag.DOCUMENT_NAME);
        query.addBoundProjection(Bean.DOCUMENT_ID);
        query.addBoundProjection(Tag.namePropertyName);
        query.getFilter().addEquals(Bean.USER_ID, user.getId());
        query.addBoundOrdering(Tag.namePropertyName);
        
        List<Bean> tags = query.projectedResults();
        List<DomainValue> result = new ArrayList<>(tags.size());
        for (Bean tag : tags) {
            result.add(new DomainValue(tag.getBizId(), (String) BindUtil.get(tag, Tag.namePropertyName)));
        }

        return result;
	}

	@Override
	public void delete(String tagId) throws Exception {
		clear(tagId);

		// Ensure that proper validation is fired
		// especially reference constraints.
		AbstractPersistence persistence = AbstractPersistence.get();
		User user = persistence.getUser();
		Customer customer = user.getCustomer();
		Module module = customer.getModule(Tag.MODULE_NAME);
		Document document = module.getDocument(customer, Tag.DOCUMENT_NAME);
		PersistentBean bean = persistence.retrieveAndLock(document, tagId);
		persistence.delete(document, bean);
	}

	@Override
	public void tag(String tagId, Iterable<Bean> beans) throws Exception {
		for (Bean bean : beans) {
			tag(tagId, bean.getBizModule(), bean.getBizDocument(), bean.getBizId());
		}
	}

	@Override
	public void untag(String tagId, Iterable<Bean> beans) throws Exception {
		for (Bean bean : beans) {
			untag(tagId, bean.getBizModule(), bean.getBizDocument(), bean.getBizId());
		}
	}

	@Override
	public void clear(String tagId) throws Exception {
		AbstractPersistence persistence = AbstractPersistence.get();
		User user = persistence.getUser();
		BizQL deleteStatement = persistence.newBizQL(String.format("delete from {%s.%s} as bean " +
																	"where bean.%s.%s = :%s " +
																	"and bean.%s = :%s",
																		Tagged.MODULE_NAME,
																		Tagged.DOCUMENT_NAME,
																		Tagged.tagPropertyName,
																		Bean.DOCUMENT_ID,
																		Bean.DOCUMENT_ID,
																		Bean.USER_ID,
																		Bean.USER_ID));
		deleteStatement.putParameter(Bean.DOCUMENT_ID, tagId);
		deleteStatement.putParameter(Bean.USER_ID, user.getId());

		deleteStatement.execute();
	}

	@Override
	@SuppressWarnings("resource")
	public AutoClosingIterable<Bean> iterate(String tagId) throws Exception {
		AbstractPersistence persistence = AbstractPersistence.get();
		User user = persistence.getUser();
		BizQL query = persistence.newBizQL(String.format("select bean.%s as %s, " + 
																"bean.%s as %s, " + 
																"bean.%s as %s " +
															"from {%s.%s} as bean " + 
															"where bean.%s.%s = :%s " +
															"and bean.%s = :%s",
																Tagged.taggedModulePropertyName,
																Tagged.taggedModulePropertyName,
																Tagged.taggedDocumentPropertyName,
																Tagged.taggedDocumentPropertyName,
																Tagged.taggedBizIdPropertyName,
																Tagged.taggedBizIdPropertyName,
																Tagged.MODULE_NAME,
																Tagged.DOCUMENT_NAME,
																Tagged.tagPropertyName,
																Bean.DOCUMENT_ID,
																Bean.DOCUMENT_ID,
																Bean.USER_ID,
																Bean.USER_ID));
		query.putParameter(Bean.DOCUMENT_ID, tagId);
		query.putParameter(Bean.USER_ID, user.getId());

		AutoClosingIterable<Bean> i = query.projectedIterable();
		return new TaggedIterable(i);
	}
}
