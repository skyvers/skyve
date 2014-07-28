package org.skyve.wildcat.util;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.messages.DomainException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.persistence.BizQL;
import org.skyve.persistence.DocumentQuery;
import org.skyve.wildcat.bind.BindUtil;
import org.skyve.wildcat.persistence.AbstractPersistence;

public final class TagUtil {
	
	private TagUtil() {
		// disallow instantiation
	}

	/**
	 * Tag 1 bean.
	 * 
	 * @param tagId Tag the bean against this tag.
	 * @param bean	The bean to be tagged.
	 * @throws Exception
	 */
	public static void tag(String tagId, Bean bean)
	throws Exception {
		tag(tagId, bean.getBizModule(), bean.getBizDocument(), bean.getBizId());
	}
	
	/**
	 * Tag 1 bean.
	 * 
	 * @param tagId	Tag the bean against this tag.
	 * @param taggedModuleName	The module name of the bean to be tagged.
	 * @param taggedDocumentName The document name of the bean to be tagged.
	 * @param taggedBizId	The bizId of the bean to be tagged.
	 * @throws Exception
	 */
	public static void tag(String tagId,
							String taggedModuleName,
							String taggedDocumentName,
							String taggedBizId)
	throws Exception {
		AbstractPersistence persistence = AbstractPersistence.get();
		User user = persistence.getUser();
		Customer customer = user.getCustomer();
		Module adminModule = customer.getModule("admin");
		Document tagDocument = adminModule.getDocument(customer, "Tag");
		Document taggedDocument = adminModule.getDocument(customer, "Tagged");
		PersistentBean tag = persistence.retrieve(tagDocument, tagId, false);

		PersistentBean tagged = taggedDocument.newInstance(user);
		BindUtil.set(tagged, "tag", tag);
		BindUtil.set(tagged, "taggedModule", taggedModuleName);
		BindUtil.set(tagged, "taggedDocument", taggedDocumentName);
		BindUtil.set(tagged, "taggedBizId", taggedBizId);

		try {
			persistence.preFlush(taggedDocument, tagged);
			persistence.upsertBeanTuple(tagged);
			persistence.postFlush(taggedDocument, tagged);
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
	 * Remove the bean from this tag.
	 * 
	 * @param tagId	The tag.
	 * @param bean	The bean to untag.
	 * @throws Exception
	 */
	public static void untag(String tagId, Bean bean)
	throws Exception {
		untag(tagId, bean.getBizModule(), bean.getBizDocument(), bean.getBizId());
	}

	/**
	 * Remove the bean from this tag.
	 * 
	 * @param tagId	The tag.
	 * @param taggedModuleName	The module name of the bean to be untagged.
	 * @param taggedDocumentName The document name of the bean to be untagged.
	 * @param taggedBizId	The bizId of the bean to be untagged.
	 * @throws Exception
	 */
	public static void untag(String tagId,
								String taggedModuleName,
								String taggedDocumentName,
								String taggedBizId)
	throws Exception {
		AbstractPersistence persistence = AbstractPersistence.get();
		User user = persistence.getUser();

		BizQL deleteStatement = persistence.newBizQL("delete from {admin.Tagged} as bean " +
														"where bean.tag.bizId = :tagId " +
														"and bean.bizUserId = :bizUserId " +
														"and bean.taggedModule = :taggedModule " +
														"and bean.taggedDocument = :taggedDocument " +
														"and bean.taggedBizId = :taggedBizId");
		deleteStatement.putParameter("tagId", tagId);
		deleteStatement.putParameter("bizUserId", user.getId());
		deleteStatement.putParameter("taggedModule", taggedModuleName);
		deleteStatement.putParameter("taggedDocument", taggedDocumentName);
		deleteStatement.putParameter("taggedBizId", taggedBizId);

		persistence.executeDML(deleteStatement);
	}

	/**
	 * Create a tag.
	 * 
	 * @param tagName	The name of the tag.
	 * @param visible	Whether the tag should be shown throughout the wildcat UI.
	 * @return	The tagId of the created tag.
	 * @throws Exception
	 */
	public static String create(String tagName, boolean visible)
	throws Exception {
	    AbstractPersistence persistence = AbstractPersistence.get();
        User user = persistence.getUser();
        Customer customer = user.getCustomer();
        Module adminModule = customer.getModule("admin");
        Document tagDocument = adminModule.getDocument(customer, "Tag");
        
        PersistentBean tag = tagDocument.newInstance(user);
        BindUtil.set(tag, "name", tagName);
        BindUtil.set(tag, "visible", Boolean.valueOf(visible));
        tag = persistence.save(tagDocument, tag);
        
        return tag.getBizId();
	}

	/**
	 * Retrieve the tagId of the named tag.
	 * 
	 * @param tagName  The name of the tag to retrieve.
	 * @return	The corresponding tagId
	 * @throws Exception
	 */
	public static String getTagId(String tagName)
	throws Exception {
	    AbstractPersistence persistence = AbstractPersistence.get();
        User user = persistence.getUser();

        DocumentQuery query = persistence.newDocumentQuery("admin", "Tag");
        query.addBoundProjection(Bean.DOCUMENT_ID);
        query.getFilter().addEquals("name", tagName);
        query.getFilter().addEquals(Bean.USER_ID, user.getId());

        List<Bean> results = persistence.retrieve(query);
        String result = null;
        if (! results.isEmpty()) {
        	result = (String) BindUtil.get(results.get(0), Bean.DOCUMENT_ID);
        }
        
        return result;
	}

	public static List<DomainValue> getTags()
	throws Exception {
	    AbstractPersistence persistence = AbstractPersistence.get();
        User user = persistence.getUser();
        
        DocumentQuery query = persistence.newDocumentQuery("admin", "Tag");
        query.addBoundProjection(Bean.DOCUMENT_ID);
        query.addBoundProjection("name");
        query.getFilter().addEquals(Bean.USER_ID, user.getId());
        query.addOrdering("name");
        
        List<Bean> tags = persistence.retrieve(query);
        List<DomainValue> result = new ArrayList<>(tags.size());
        for (Bean tag : tags) {
            result.add(new DomainValue(tag.getBizId(), (String) BindUtil.get(tag, "name")));
        }

        return result;
	}
	
	/**
	 * Delete a tag.
	 * 
	 * @param tagId	The tagId of the tag to delete.
	 * @throws Exception
	 */
	public static void delete(String tagId)
	throws Exception {
		clear(tagId);

		// Ensure that proper validation is fired
		// especially reference constraints.
		AbstractPersistence persistence = AbstractPersistence.get();
		User user = persistence.getUser();
		Customer customer = user.getCustomer();
		Module module = customer.getModule("admin");
		Document document = module.getDocument(customer, "Tag");
		PersistentBean bean = persistence.retrieve(document, tagId, true);
		persistence.delete(document, bean);
	}

	/**
	 * Tag a bunch of beans.
	 * 
	 * @param tagId	The tag to use.
	 * @param beans	The beans to tag.
	 * @throws Exception
	 */
	public static void tag(String tagId, Iterable<Bean> beans)
	throws Exception {
		for (Bean bean : beans) {
			tag(tagId, bean.getBizModule(), bean.getBizDocument(), bean.getBizId());
		}
	}

	/**
	 * Untag (remove) a bunch of beans.
	 * 
	 * @param tagId	The tag to remove from.
	 * @param beans	The beans to untag.
	 * @throws Exception
	 */
	public static void untag(String tagId, Iterable<Bean> beans)
	throws Exception {
		for (Bean bean : beans) {
			untag(tagId, bean.getBizModule(), bean.getBizDocument(), bean.getBizId());
		}
	}

	/**
	 * Clear any beans related to the given tag.
	 * 
	 * @param tagId	the given tag
	 * @throws Exception
	 */
	public static void clear(String tagId) 
	throws Exception {
		AbstractPersistence persistence = AbstractPersistence.get();
		User user = persistence.getUser();
		BizQL deleteStatement = persistence.newBizQL("delete from {admin.Tagged} as bean " +
														"where bean.tag.bizId = :tagId " +
														"and bean.bizUserId = :bizUserId");
		deleteStatement.putParameter("tagId", tagId);
		deleteStatement.putParameter("bizUserId", user.getId());

		persistence.executeDML(deleteStatement);
	}

	private static class TaggedIterable implements Iterable<Bean> {
		private Iterator<Bean> taggedIterator;
		
		private TaggedIterable(Iterator<Bean> taggedIterator) {
			this.taggedIterator = taggedIterator;
		}

		private static class TaggedIterator implements Iterator<Bean> {
			private Iterator<Bean> taggedIterator;
			
			private Bean nextBean;
			
			private TaggedIterator(Iterator<Bean> taggedIterator) {
				this.taggedIterator = taggedIterator;
			}
			
			@Override
			public boolean hasNext() {
				boolean result = taggedIterator.hasNext();
				if (result) {
	
					Bean tagged = taggedIterator.next();
					nextBean = null;
	
					String taggedModule = null;
					String taggedDocument = null;
					String taggedBizId = null;
					try {
						taggedModule = (String) BindUtil.get(tagged, "taggedModule");
						taggedDocument = (String) BindUtil.get(tagged, "taggedDocument");
						taggedBizId = (String) BindUtil.get(tagged, "taggedBizId");
						AbstractPersistence persistence = AbstractPersistence.get(); // thread local remember
						Customer customer = persistence.getUser().getCustomer();
						Module module = customer.getModule(taggedModule);
						Document document = module.getDocument(customer, taggedDocument);
						nextBean = persistence.retrieve(document, taggedBizId, false);
						if (nextBean == null) {
							throw new Exception("Tagged item does not exist");
						}
					}
					catch (Exception e) {
						StringBuilder sb = new StringBuilder(256);
						sb.append(taggedModule).append('.').append(taggedDocument);
						sb.append('.').append(taggedBizId).append(" - ").append(e.getLocalizedMessage());
						System.err.println(sb.toString());
						// try the next one
						result = hasNext();
					}
				}
				
				return result;
			}

			@Override
			public Bean next() {
				Bean result = nextBean;
				nextBean = null;
				
				return result;
			}

			@Override
			public void remove() {
				throw new IllegalAccessError("Cannot remove from a TaggedIterator.");
			}
		}
		
		@Override
		@SuppressWarnings("synthetic-access")
		public Iterator<Bean> iterator() {
			return new TaggedIterator(taggedIterator);
		}
	}

	/**
	 * Iterate over the tagged beans.
	 * @param tagId	The tag to iterate.
	 * @return	The beans (a scrolled set).  Each bean is loaded into 1st level cache so beware.
	 * @throws Exception
	 */
	@SuppressWarnings("synthetic-access")
	public static Iterable<Bean> iterateTagged(String tagId)
	throws Exception {
		AbstractPersistence persistence = AbstractPersistence.get();
		User user = persistence.getUser();
		BizQL query = persistence.newBizQL("select bean.taggedModule as taggedModule, " + 
												"bean.taggedDocument as taggedDocument, " + 
												"bean.taggedBizId as taggedBizId " +
											"from {admin.Tagged} as bean " + 
											"where bean.tag.bizId = :tagId " +
											"and bean.bizUserId = :bizUserId");
		query.putParameter("tagId", tagId);
		query.putParameter("bizUserId", user.getId());

		return new TaggedIterable(persistence.iterate(query).iterator());
	}
}
