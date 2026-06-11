package org.skyve.impl.tag;

import java.util.ArrayList;
import java.util.List;

import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.app.AppConstants;
import org.skyve.domain.app.admin.Tag;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.messages.SecurityException;
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

/**
 * Implements tag lifecycle operations for the current user using admin tag documents.
 *
 * <p>Threading: this singleton is stateless and delegates all stateful work to the
 * request-bound persistence context.
 */
public class DefaultTagManager implements TagManager {
	private static final DefaultTagManager INSTANCE = new DefaultTagManager();

	private static final String TAG_ATTRIBUTE_QUERY_PREFIX = "where bean.%s.%s = :%s ";
	private static final String TAG_QUERY_AND_WITH_SPACE = "and bean.%s = :%s ";
	private static final String TAG_QUERY_AND = "and bean.%s = :%s";
	
	/**
	 * Returns the singleton instance.
	 */
	public static DefaultTagManager get() {
		return INSTANCE;
	}
	
	/**
	 * Disallows external instantiation.
	 */
	private DefaultTagManager() {
		// nothing to see here
	}
	
	/**
	 * Applies a tag to a bean.
	 *
	 * @param tagId The tag document id.
	 * @param bean The bean to tag.
	 * @throws Exception If tagging fails.
	 */
	@Override
	public void tag(String tagId, Bean bean) throws Exception {
		tag(tagId, bean.getBizModule(), bean.getBizDocument(), bean.getBizId());
	}

	/**
	 * Applies a tag to a specific bean identity tuple.
	 *
	 * @param tagId The tag document id.
	 * @param taggedModuleName The module containing the tagged document.
	 * @param taggedDocumentName The tagged document name.
	 * @param taggedBizId The tagged bean id.
	 * @throws Exception If persistence or validation fails.
	 */
	@Override
	public void tag(String tagId, String taggedModuleName, String taggedDocumentName, String taggedBizId) throws Exception {
		AbstractPersistence persistence = AbstractPersistence.get();
		User user = persistence.getUser();
		Customer customer = user.getCustomer();
		Module adminModule = customer.getModule(AppConstants.ADMIN_MODULE_NAME);
		Document tagDocument = adminModule.getDocument(customer, AppConstants.TAG_DOCUMENT_NAME);
		Document taggedDocument = adminModule.getDocument(customer, AppConstants.TAGGED_DOCUMENT_NAME);
		PersistentBean tag = persistence.retrieve(tagDocument, tagId);

		PersistentBean tagged = taggedDocument.newInstance(user);
		BindUtil.set(tagged, AppConstants.TAG_ATTRIBUTE_NAME, tag);
		BindUtil.set(tagged, AppConstants.TAGGED_MODULE_ATTRIBUTE_NAME, taggedModuleName);
		BindUtil.set(tagged, AppConstants.TAGGED_DOCUMENT_ATTRIBUTE_NAME, taggedDocumentName);
		BindUtil.set(tagged, AppConstants.TAGGED_BIZID_ATTRIBUTE_NAME, taggedBizId);

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

	/**
	 * Removes a tag from a bean.
	 *
	 * @param tagId The tag document id.
	 * @param bean The bean to untag.
	 * @throws Exception If untagging fails.
	 */
	@Override
	public void untag(String tagId, Bean bean) throws Exception {
		untag(tagId, bean.getBizModule(), bean.getBizDocument(), bean.getBizId());
	}

	/**
	 * Removes a tag association for a specific bean identity tuple.
	 *
	 * @param tagId The tag document id.
	 * @param taggedModuleName The module containing the tagged document.
	 * @param taggedDocumentName The tagged document name.
	 * @param taggedBizId The tagged bean id.
	 * @throws Exception If persistence operations fail.
	 */
	@Override
	public void untag(String tagId, String taggedModuleName, String taggedDocumentName, String taggedBizId) throws Exception {
		AbstractPersistence persistence = AbstractPersistence.get();
		User user = persistence.getUser();

		BizQL deleteStatement = persistence.newBizQL(String.format("delete from {%s.%s} as bean " +
																	TAG_ATTRIBUTE_QUERY_PREFIX +
																	TAG_QUERY_AND_WITH_SPACE +
																	TAG_QUERY_AND_WITH_SPACE +
																	TAG_QUERY_AND_WITH_SPACE +
																	TAG_QUERY_AND,
																		AppConstants.ADMIN_MODULE_NAME,
																		AppConstants.TAGGED_DOCUMENT_NAME,
																		AppConstants.TAG_ATTRIBUTE_NAME,
																		Bean.DOCUMENT_ID,
																		Bean.DOCUMENT_ID,
																		Bean.USER_ID,
																		Bean.USER_ID,
																		AppConstants.TAGGED_MODULE_ATTRIBUTE_NAME,
																		AppConstants.TAGGED_MODULE_ATTRIBUTE_NAME,
																		AppConstants.TAGGED_DOCUMENT_ATTRIBUTE_NAME,
																		AppConstants.TAGGED_DOCUMENT_ATTRIBUTE_NAME,
																		AppConstants.TAGGED_BIZID_ATTRIBUTE_NAME,
																		AppConstants.TAGGED_BIZID_ATTRIBUTE_NAME));
		deleteStatement.putParameter(Bean.DOCUMENT_ID, tagId);
		deleteStatement.putParameter(Bean.USER_ID, user.getId());
		deleteStatement.putParameter(AppConstants.TAGGED_MODULE_ATTRIBUTE_NAME, taggedModuleName);
		deleteStatement.putParameter(AppConstants.TAGGED_DOCUMENT_ATTRIBUTE_NAME, taggedDocumentName);
		deleteStatement.putParameter(AppConstants.TAGGED_BIZID_ATTRIBUTE_NAME, taggedBizId);

		deleteStatement.execute();
	}

	/**
	 * Creates a new user-owned tag.
	 *
	 * @param tagName The tag display name.
	 * @param visible Whether the tag is visible in UI selectors.
	 * @return The created tag biz id.
	 * @throws Exception If creation fails.
	 */
	@Override
	public String create(String tagName, boolean visible) throws Exception {
	    AbstractPersistence persistence = AbstractPersistence.get();
        User user = persistence.getUser();
        Customer customer = user.getCustomer();
        Module adminModule = customer.getModule(AppConstants.ADMIN_MODULE_NAME);
        Document tagDocument = adminModule.getDocument(customer, AppConstants.TAG_DOCUMENT_NAME);
        
        Tag tag = tagDocument.newInstance(user);
        tag.setName(tagName);
        tag.setVisible(Boolean.valueOf(visible));
        tag = persistence.save(tagDocument, tag);
        
        return tag.getBizId();
	}

	/**
	 * Resolves the current user's tag id by tag name.
	 *
	 * @param tagName The tag display name.
	 * @return The tag id, or {@code null} when no matching tag exists.
	 * @throws Exception If lookup fails.
	 */
	@Override
	public String getTagId(String tagName) throws Exception {
	    AbstractPersistence persistence = AbstractPersistence.get();
        User user = persistence.getUser();

        DocumentQuery query = persistence.newDocumentQuery(AppConstants.ADMIN_MODULE_NAME, AppConstants.TAG_DOCUMENT_NAME);
        query.addBoundProjection(Bean.DOCUMENT_ID);
        DocumentFilter filter = query.getFilter();
        filter.addEquals(AppConstants.NAME_ATTRIBUTE_NAME, tagName);
        filter.addEquals(Bean.USER_ID, user.getId());

        List<Bean> results = query.projectedResults();
        String result = null;
        if (! results.isEmpty()) {
        	result = (String) BindUtil.get(results.get(0), Bean.DOCUMENT_ID);
        }
        
        return result;
	}

	/**
	 * Lists all tags owned by the current user ordered by name.
	 *
	 * @return Domain values where code is tag id and description is tag name.
	 * @throws Exception If retrieval fails.
	 */
	@Override
	public List<DomainValue> getTags() throws Exception {
	    AbstractPersistence persistence = AbstractPersistence.get();
        User user = persistence.getUser();
        
        DocumentQuery query = persistence.newDocumentQuery(AppConstants.ADMIN_MODULE_NAME, AppConstants.TAG_DOCUMENT_NAME);
        query.addBoundProjection(Bean.DOCUMENT_ID);
        query.addBoundProjection(AppConstants.NAME_ATTRIBUTE_NAME);
        query.getFilter().addEquals(Bean.USER_ID, user.getId());
        query.addBoundOrdering(AppConstants.NAME_ATTRIBUTE_NAME);
        
        List<Bean> tags = query.projectedResults();
        List<DomainValue> result = new ArrayList<>(tags.size());
        for (Bean tag : tags) {
            result.add(new DomainValue(tag.getBizId(), (String) BindUtil.get(tag, AppConstants.NAME_ATTRIBUTE_NAME)));
        }

        return result;
	}

	/**
	 * Deletes a tag after clearing all current-user associations.
	 *
	 * @param tagId The tag document id.
	 * @throws Exception If delete validation or persistence fails.
	 */
	@Override
	public void delete(String tagId) throws Exception {
		clear(tagId);

		// Ensure that proper validation is fired - especially reference constraints.
		AbstractPersistence persistence = AbstractPersistence.get();
		User user = persistence.getUser();
		Customer customer = user.getCustomer();
		Module module = customer.getModule(AppConstants.ADMIN_MODULE_NAME);
		Document document = module.getDocument(customer, AppConstants.TAG_DOCUMENT_NAME);
		PersistentBean bean = persistence.retrieveAndLock(document, tagId);
		// Check the tag is for the current user
		if (! user.getId().equals(bean.getBizUserId())) {
			throw new SecurityException("delete this tag", user.getName());
		}
		persistence.delete(document, bean);
	}

	/**
	 * Applies a tag to all beans in an iterable.
	 *
	 * @param tagId The tag document id.
	 * @param beans Beans to tag.
	 * @throws Exception If tagging fails for any bean.
	 */
	@Override
	public void tag(String tagId, Iterable<Bean> beans) throws Exception {
		for (Bean bean : beans) {
			tag(tagId, bean.getBizModule(), bean.getBizDocument(), bean.getBizId());
		}
	}

	/**
	 * Removes a tag from all beans in an iterable.
	 *
	 * @param tagId The tag document id.
	 * @param beans Beans to untag.
	 * @throws Exception If untagging fails for any bean.
	 */
	@Override
	public void untag(String tagId, Iterable<Bean> beans) throws Exception {
		for (Bean bean : beans) {
			untag(tagId, bean.getBizModule(), bean.getBizDocument(), bean.getBizId());
		}
	}

	/**
	 * Removes all tagged-document rows for the current user and tag.
	 *
	 * @param tagId The tag document id.
	 * @throws Exception If deletion fails.
	 */
	@Override
	public void clear(String tagId) throws Exception {
		AbstractPersistence persistence = AbstractPersistence.get();
		User user = persistence.getUser();
		BizQL deleteStatement = persistence.newBizQL(String.format("delete from {%s.%s} as bean " +
																	TAG_ATTRIBUTE_QUERY_PREFIX +
																	TAG_QUERY_AND,
																		AppConstants.ADMIN_MODULE_NAME,
																		AppConstants.TAGGED_DOCUMENT_NAME,
																		AppConstants.TAG_ATTRIBUTE_NAME,
																		Bean.DOCUMENT_ID,
																		Bean.DOCUMENT_ID,
																		Bean.USER_ID,
																		Bean.USER_ID));
		deleteStatement.putParameter(Bean.DOCUMENT_ID, tagId);
		deleteStatement.putParameter(Bean.USER_ID, user.getId());

		deleteStatement.execute();
	}

	/**
	 * Iterates bean identities currently associated with a tag for the current user.
	 *
	 * @param tagId The tag document id.
	 * @return A lazily-read iterable of tagged bean identities.
	 * @throws Exception If query execution fails.
	 */
	@Override
	@SuppressWarnings("resource")
	public AutoClosingIterable<Bean> iterate(String tagId) throws Exception {
		AbstractPersistence persistence = AbstractPersistence.get();
		User user = persistence.getUser();
		BizQL query = persistence.newBizQL(String.format("select bean.%s as %s, " + 
																"bean.%s as %s, " + 
																"bean.%s as %s " +
															"from {%s.%s} as bean " + 
															TAG_ATTRIBUTE_QUERY_PREFIX +
															TAG_QUERY_AND,
																AppConstants.TAGGED_MODULE_ATTRIBUTE_NAME,
																AppConstants.TAGGED_MODULE_ATTRIBUTE_NAME,
																AppConstants.TAGGED_DOCUMENT_ATTRIBUTE_NAME,
																AppConstants.TAGGED_DOCUMENT_ATTRIBUTE_NAME,
																AppConstants.TAGGED_BIZID_ATTRIBUTE_NAME,
																AppConstants.TAGGED_BIZID_ATTRIBUTE_NAME,
																AppConstants.ADMIN_MODULE_NAME,
																AppConstants.TAGGED_DOCUMENT_NAME,
																AppConstants.TAG_ATTRIBUTE_NAME,
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
