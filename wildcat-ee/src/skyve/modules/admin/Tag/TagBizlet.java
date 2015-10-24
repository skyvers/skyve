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
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.document.Bizlet;
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

	public static String WILDCAT_SAVE_ACTION = "WILDCATResave";
	public static String WILDCAT_DELETE_ACTION = "WILDCATDelete";
	public static String WILDCAT_VALIDATE_ACTION = "WILDCATValidate";

	@Override
	public List<DomainValue> getDynamicDomainValues(String attributeName, Tag bean) throws Exception {

		Persistence pers = CORE.getPersistence();
		List<DomainValue> result = new ArrayList<>();

		Customer customer = pers.getUser().getCustomer();

		if (Tag.documentNamePropertyName.equals(attributeName)) {
			if (bean.getModuleName() != null) {
				Module module = customer.getModule(bean.getModuleName());
				for (String documentName : module.getDocumentRefs().keySet()) {
					Document document = module.getDocument(customer, documentName);
					result.add(new DomainValue(document.getName(), document.getDescription()));
				}
			}
		}

		if (Tag.attributeNamePropertyName.equals(attributeName)) {
			if (bean.getModuleName() != null && bean.getDocumentName() != null) {
				Module module = customer.getModule(bean.getModuleName());
				Document document = module.getDocument(customer, bean.getDocumentName());
				for (Attribute attribute : document.getAllAttributes()) {
					result.add(new DomainValue(attribute.getName(), attribute.getDisplayName()));
				}
			}
		}

		if (Tag.actionTagPropertyName.equals(attributeName)) {

			// look for OTHER tags
			DocumentQuery q = pers.newDocumentQuery(Tag.MODULE_NAME, Tag.DOCUMENT_NAME);
			q.getFilter().addNotEquals(Bean.DOCUMENT_ID, bean.getBizId());
			q.addOrdering(Tag.namePropertyName);

			List<Tag> tags = q.beanResults();
			for (Tag t : tags) {
				result.add(new DomainValue(t.getBizId(), t.getName()));
			}
		}

		if (Tag.documentActionPropertyName.equals(attributeName)) {
			if (bean.getModuleName() != null && bean.getDocumentName() != null) {

				Module module = customer.getModule(bean.getModuleName());
				Document document = module.getDocument(customer, bean.getDocumentName());
				for (String act : document.getDefinedActionNames()) {
					result.add(new DomainValue(act));
				}
				// add default save action
				result.add(new DomainValue(WILDCAT_SAVE_ACTION, "Save Documents"));
				result.add(new DomainValue(WILDCAT_DELETE_ACTION, "Delete Documents"));
				result.add(new DomainValue(WILDCAT_VALIDATE_ACTION, "Validate Documents"));
			}
		}

		if (Tag.documentConditionPropertyName.equals(attributeName)) {
			if (bean.getModuleName() != null && bean.getDocumentName() != null) {
				Module module = customer.getModule(bean.getModuleName());
				Document document = module.getDocument(customer, bean.getDocumentName());
				for (String act : document.getConditionNames()) {
					result.add(new DomainValue(act));
				}
			}
		}

		return result;
	}

	@Override
	public Tag preExecute(ImplicitActionName actionName, Tag bean, Bean parentBean, WebContext webContext) throws Exception {
		if (ImplicitActionName.Edit.equals(actionName)) {
			if (bean.getModuleName() == null) {
				Module homeModule = CORE.getUser().getCustomer().getHomeModule();
				bean.setModuleName(homeModule.getName());
				bean.setDocumentName(homeModule.getHomeDocumentName());
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

		return bean;
	}

	@Override
	public List<DomainValue> getVariantDomainValues(String attributeName) throws Exception {

		List<DomainValue> result = new ArrayList<>();

		Customer customer = CORE.getUser().getCustomer();
		if (Tag.moduleNamePropertyName.equals(attributeName)) {
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
			intersect.append(" and tag_id =subjectTagId");
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
				//EXT method handles if this bean was not tagged
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
		Persistence pers = CORE.getPersistence();
		User user = pers.getUser();
		Customer customer = user.getCustomer();
		Module module = customer.getModule(moduleName);
		Document document = module.getDocument(customer, documentName);

		List<Bean> beans = new ArrayList<>();
		for (Bean bean : EXT.iterateTagged(tag.getBizId())) {
			if (bean != null && bean.getBizModule().equals(module.getName()) && bean.getBizDocument().equals(document.getName())) {
				// need to check that this is only done for documents of the
				// selected type
				beans.add(bean);
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
