package modules.admin.Tag;

import java.util.ArrayList;
import java.util.List;

import modules.admin.domain.Tag;
import modules.admin.domain.Tag.FilterAction;
import modules.admin.domain.Tag.FilterOperator;
import modules.admin.domain.Tagged;

import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.domain.Bean;
import org.skyve.impl.util.TagUtil;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.model.document.Condition;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.DocumentQuery.AggregateFunction;
import org.skyve.persistence.Persistence;
import org.skyve.persistence.SQL;
import org.skyve.web.WebContext;

public class TagBizlet extends Bizlet<Tag> {

	private static final long serialVersionUID = -927602139528710862L;

	public static final String SYSTEM_TAG_ACTION_NOTIFICATION = "SYSTEM Tag Action Notification";
	public static final String SYSTEM_TAG_ACTION_DEFAULT_SUBJECT = "Perform Document Action for Tag - Complete";
	public static final String SYSTEM_TAG_ACTION_DEFAULT_BODY = "The action for Tag {name} is complete.";


	@Override
	public List<DomainValue> getDynamicDomainValues(String attributeName, Tag bean) throws Exception {

		Persistence pers = CORE.getPersistence();
		List<DomainValue> result = new ArrayList<>();

		Customer customer = pers.getUser().getCustomer();

		if (Tag.actionDocumentNamePropertyName.equals(attributeName)) {
			if (bean.getActionModuleName() != null) {
				Module module = customer.getModule(bean.getActionModuleName());
				for (String documentName : module.getDocumentRefs().keySet()) {
					Document document = module.getDocument(customer, documentName);
					result.add(new DomainValue(document.getName(), document.getDescription()));
				}
			}
		}

		if (Tag.uploadDocumentNamePropertyName.equals(attributeName)) {
			if (bean.getUploadModuleName() != null) {
				Module module = customer.getModule(bean.getUploadModuleName());
				for (String documentName : module.getDocumentRefs().keySet()) {
					Document document = module.getDocument(customer, documentName);
					result.add(new DomainValue(document.getName(), document.getDescription()));
				}
			}
		}

		if (Tag.attributeNamePropertyName.equals(attributeName)) {
			if (bean.getUploadModuleName() != null && bean.getUploadDocumentName() != null) {
				Module module = customer.getModule(bean.getUploadModuleName());
				Document document = module.getDocument(customer, bean.getUploadDocumentName());
				for (Attribute attribute : document.getAllAttributes()) {
					result.add(new DomainValue(attribute.getName(), attribute.getDisplayName()));
				}
			}
		}

		if (Tag.actionTagPropertyName.equals(attributeName)) {

			// look for OTHER tags
			DocumentQuery q = pers.newDocumentQuery(Tag.MODULE_NAME, Tag.DOCUMENT_NAME);
			q.getFilter().addNotEquals(Bean.DOCUMENT_ID, bean.getBizId());
			q.addBoundOrdering(Tag.namePropertyName);

			List<Tag> tags = q.beanResults();
			for (Tag t : tags) {
				result.add(new DomainValue(t.getBizId(), t.getName()));
			}
		}

		if (Tag.documentActionPropertyName.equals(attributeName)) {
			if (bean.getActionModuleName() != null && bean.getActionDocumentName() != null) {

				Module module = customer.getModule(bean.getActionModuleName());
				Document document = module.getDocument(customer, bean.getActionDocumentName());
				for (String act : document.getDefinedActionNames()) {
					result.add(new DomainValue(act));
				}

				// add default save action
				result.addAll(TagDefaultAction.toDomainValues());
			}
		}

		if (Tag.documentConditionPropertyName.equals(attributeName)) {
			if (bean.getActionModuleName() != null && bean.getActionDocumentName() != null) {
				Module module = customer.getModule(bean.getActionModuleName());
				Document document = module.getDocument(customer, bean.getActionDocumentName());
				for (String act : document.getConditionNames()) {
					Condition condition = document.getCondition(act);
					result.add(new DomainValue(act, (condition.getDescription() == null ? act : condition.getDescription())));
				}
			}
		}

		return result;
	}

	@Override
	public Tag preExecute(ImplicitActionName actionName, Tag bean, Bean parentBean, WebContext webContext) throws Exception {
		if (ImplicitActionName.Edit.equals(actionName)) {
			if (bean.getUploadModuleName() == null) {
				Module homeModule = CORE.getUser().getCustomer().getHomeModule();
				bean.setUploadModuleName(homeModule.getName());
				bean.setUploadDocumentName(homeModule.getHomeDocumentName());
			}
			if (bean.getActionModuleName() == null) {
				Module homeModule = CORE.getUser().getCustomer().getHomeModule();
				bean.setActionModuleName(homeModule.getName());
				bean.setActionDocumentName(homeModule.getHomeDocumentName());
			}
			if (bean.getFileHasHeaders() == null) {
				bean.setFileHasHeaders(Boolean.TRUE);
			}
			if (bean.getFilterOperator() == null) {
				bean.setFilterOperator(FilterOperator.equals);
			}
			if (bean.getFilterAction() == null) {
				bean.setFilterAction(FilterAction.tagRecordsThatMatch);
			}
			if (bean.getFilterColumn() == null) {
				bean.setFilterColumn(new Integer(1));
			}
			bean.setCopyToUserTagName(bean.getName());

			update(bean);
			bean.originalValues().clear();
		}

		if (ImplicitActionName.Delete.equals(actionName)) {
			TagUtil.clear(bean.getBizId());
		}

		return bean;
	}

	@Override
	public List<DomainValue> getVariantDomainValues(String attributeName) throws Exception {

		List<DomainValue> result = new ArrayList<>();

		Customer customer = CORE.getUser().getCustomer();
		if (Tag.uploadModuleNamePropertyName.equals(attributeName) || Tag.actionModuleNamePropertyName.equals(attributeName)) {
			for (Module module : customer.getModules()) {
				result.add(new DomainValue(module.getName(), module.getTitle()));
			}
		}

		return result;
	}

	public static void update(Tag bean) throws Exception {
		// Update current Tag
		Integer result = getCount(bean);

		bean.setNumberTagged(result);
		bean.setCurrentTagCount(result);

		bean.setActionTagCount(TagBizlet.getCount(bean.getActionTag()));
	}

	/**
	 * Add to the subject tag records from the object tag.
	 * 
	 * @param subject
	 * @param object
	 */
	public static void union(Tag subject, Tag object) throws Exception {

		if (subject != null && object != null) {

			// no dialect insensitive way to use SQL for creating new UUIDs for
			// new records
			// add tagged items from object tag to subject tag
			// EXT function deals with duplicates
			for (Bean bean : EXT.iterateTagged(object.getBizId())) {
				EXT.tag(subject.getBizId(), bean);
			}
		}

	}

	/**
	 * Deletes from the subject Tag any items which are not also in the object
	 * Tag.
	 * 
	 * @param subject
	 * @param object
	 * @throws Exception
	 */
	public static void intersect(Tag subject, Tag object) throws Exception {

		if (subject != null && object != null) {
			Persistence pers = CORE.getPersistence();

			// insecure SQL for performance
			StringBuilder intersect = new StringBuilder();
			intersect.append("delete from ADM_Tagged ");
			intersect.append(" where taggedBizId not in (");
			intersect.append(" select taggedBizId from ADM_Tagged where ");
			intersect.append(" tag_id = :objectTagId");
			intersect.append(" and bizCustomer=:objectBizCustomer").append(" ) ");
			intersect.append(" and tag_id =:subjectTagId");
			intersect.append(" and bizCustomer=:subjectBizCustomer");

			SQL sql = pers.newSQL(intersect.toString());
			sql.putParameter("objectTagId", object.getBizId(), false);
			sql.putParameter("subjectTagId", subject.getBizId(), false);
			sql.putParameter("objectBizCustomer", subject.getBizCustomer(), false);
			sql.putParameter("subjectBizCustomer", subject.getBizCustomer(), false);

			sql.execute();
		}
	}

	/**
	 * Deletes from the subject Tag items which are in the object Tag.
	 * 
	 * @param subject
	 * @param object
	 * @throws Exception
	 */
	public static void except(Tag subject, Tag object) throws Exception {

		if (subject != null && object != null) {
			for (Bean bean : EXT.iterateTagged(object.getBizId())) {
				// EXT method handles if this bean was not tagged
				EXT.untag(subject.getBizId(), bean);
			}

		}
	}

	/**
	 * Retrieve the items tagged which match the specified module and document
	 * 
	 * @param mailout
	 * @return
	 * @throws Exception
	 */
	public static List<Bean> getTaggedItemsForDocument(Tag tag, String moduleName, String documentName) throws Exception {
		List<Bean> beans = new ArrayList<>();
		if (moduleName != null && documentName != null) {
			Persistence pers = CORE.getPersistence();
			User user = pers.getUser();
			Customer customer = user.getCustomer();
			Module module = customer.getModule(moduleName);
			Document document = module.getDocument(customer, documentName);

			if (tag != null) {
				for (Bean bean : EXT.iterateTagged(tag.getBizId())) {
					if (bean != null && bean.getBizModule().equals(module.getName()) && bean.getBizDocument().equals(document.getName())) {
						// need to check that this is only done for documents of
						// the
						// selected type
						beans.add(bean);
					}
				}
			}
		}

		return beans;
	}

	public static Long getTaggedCountForDocument(Tag tag, String moduleName, String documentName) throws Exception {
		Long result = new Long(0);

		Persistence pers = CORE.getPersistence();
		DocumentQuery q = pers.newDocumentQuery(Tagged.MODULE_NAME, Tagged.DOCUMENT_NAME);
		q.getFilter().addEquals(Tagged.tagPropertyName, tag);
		q.getFilter().addEquals(Tagged.taggedModulePropertyName, moduleName);
		q.getFilter().addEquals(Tagged.taggedDocumentPropertyName, documentName);
		q.addAggregateProjection(AggregateFunction.Count, Bean.DOCUMENT_ID, "CountOfId");

		result = q.scalarResult(Long.class);

		return result;
	}

	public static Integer getCount(Tag bean) throws Exception {
		if (bean != null) {
			Persistence persistence = CORE.getPersistence();
			DocumentQuery q = persistence.newDocumentQuery(Tagged.MODULE_NAME, Tagged.DOCUMENT_NAME);
			q.getFilter().addEquals(Tagged.tagPropertyName, bean);
			q.addAggregateProjection(AggregateFunction.Count, Tagged.taggedBizIdPropertyName, "CountOfTagged");

			return Integer.valueOf(q.retrieveScalar(Number.class).intValue());
		}
		return new Integer(0);

	}

}
