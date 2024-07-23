package modules.admin.Tag;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.domain.Bean;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.model.document.Condition;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.persistence.AutoClosingIterable;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;
import org.skyve.persistence.SQL;
import org.skyve.tag.TagManager;
import org.skyve.web.WebContext;

import modules.admin.Jobs.JobsBizlet;
import modules.admin.domain.Tag;
import modules.admin.domain.Tag.FilterAction;
import modules.admin.domain.Tag.FilterOperator;

public class TagBizlet extends Bizlet<TagExtension> {
	public static final String SYSTEM_TAG_ACTION_NOTIFICATION = "SYSTEM Tag Action Notification";
	public static final String SYSTEM_TAG_ACTION_DEFAULT_SUBJECT = "Perform Document Action for Tag - Complete";
	public static final String SYSTEM_TAG_ACTION_DEFAULT_BODY = "The action for Tag {name} is complete." + JobsBizlet.SYSTEM_JOB_NOTIFICATION_LINK_TO_JOBS;

	@Override
	public List<DomainValue> getDynamicDomainValues(String attributeName, TagExtension bean) throws Exception {
		Persistence pers = CORE.getPersistence();
		List<DomainValue> result = new ArrayList<>();

		Customer customer = pers.getUser().getCustomer();

		if (Tag.actionDocumentNamePropertyName.equals(attributeName)
				&& bean.getActionModuleName() != null) {
			Module module = customer.getModule(bean.getActionModuleName());
			for (String documentName : module.getDocumentRefs().keySet()) {
				Document document = module.getDocument(customer, documentName);
				result.add(new DomainValue(document.getName(), document.getLocalisedSingularAlias()));
			}
			result.sort(Comparator.comparing(DomainValue::getLocalisedDescription));
		}

		if (Tag.uploadDocumentNamePropertyName.equals(attributeName)
				&& bean.getUploadModuleName() != null) {
			Module module = customer.getModule(bean.getUploadModuleName());
			for (String documentName : module.getDocumentRefs().keySet()) {
				Document document = module.getDocument(customer, documentName);
				result.add(new DomainValue(document.getName(), document.getLocalisedSingularAlias()));
			}
			result.sort(Comparator.comparing(DomainValue::getLocalisedDescription));
		}

		if (Tag.attributeNamePropertyName.equals(attributeName)
				&& bean.getUploadModuleName() != null && bean.getUploadDocumentName() != null) {
			Module module = customer.getModule(bean.getUploadModuleName());
			Document document = module.getDocument(customer, bean.getUploadDocumentName());
			for (Attribute attribute : document.getAllAttributes(customer)) {
				result.add(new DomainValue(attribute.getName(), attribute.getLocalisedDisplayName()));
			}
			result.sort(Comparator.comparing(DomainValue::getLocalisedDescription));
		}

		if (Tag.operandTagPropertyName.equals(attributeName)) {
			// look for OTHER tags
			DocumentQuery q = pers.newDocumentQuery(Tag.MODULE_NAME, Tag.DOCUMENT_NAME);
			q.getFilter().addNotEquals(Bean.DOCUMENT_ID, bean.getBizId());
			q.addBoundOrdering(Tag.namePropertyName);

			List<Tag> tags = q.beanResults();
			for (Tag t : tags) {
				result.add(new DomainValue(t.getBizId(), t.getName()));
			}
			result.sort(Comparator.comparing(DomainValue::getLocalisedDescription));
		}

		if (Tag.documentActionPropertyName.equals(attributeName)
				&& bean.getActionModuleName() != null && bean.getActionDocumentName() != null) {
			Module module = customer.getModule(bean.getActionModuleName());
			Document document = module.getDocument(customer, bean.getActionDocumentName());
			for (String act : document.getDefinedActionNames()) {
				result.add(new DomainValue(act));
			}

			// add default save action
			result.addAll(TagDefaultAction.toDomainValues());
			result.sort(Comparator.comparing(DomainValue::getLocalisedDescription));
		}

		if (Tag.documentConditionPropertyName.equals(attributeName)
				&& bean.getActionModuleName() != null && bean.getActionDocumentName() != null) {
			Module module = customer.getModule(bean.getActionModuleName());
			Document document = module.getDocument(customer, bean.getActionDocumentName());
			for (String act : document.getConditionNames()) {
				Condition condition = document.getCondition(act);
				result.add(new DomainValue(act, (condition.getDescription() == null ? act : condition.getDescription())));
			}
			result.sort(Comparator.comparing(DomainValue::getLocalisedDescription));
		}

		return result;
	}

	@Override
	public TagExtension preExecute(ImplicitActionName actionName, TagExtension bean, Bean parentBean, WebContext webContext) throws Exception {
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
				bean.setFilterColumn(Integer.valueOf(1));
			}
			bean.setCopyToUserTagName(bean.getName());
			
			//set counters
			TagExtension operandTag = bean.getOperandTag();
			bean.setOperandTagCount(Long.valueOf((operandTag == null) ? 0L : operandTag.count()));
			bean.setUploadTagged(Long.valueOf(bean.countDocument(bean.getUploadModuleName(), bean.getUploadDocumentName())));
			bean.setTotalTagged(Long.valueOf(bean.count()));

			bean.originalValues().clear();
		}

		if (ImplicitActionName.Delete.equals(actionName)) {
			EXT.getTagManager().clear(bean.getBizId());
		}

		return bean;
	}

	@Override
	public List<DomainValue> getVariantDomainValues(String attributeName) throws Exception {
		List<DomainValue> result = new ArrayList<>();

		Customer customer = CORE.getUser().getCustomer();
		if (Tag.uploadModuleNamePropertyName.equals(attributeName) || Tag.actionModuleNamePropertyName.equals(attributeName)) {
			for (Module module : customer.getModules()) {
				result.add(new DomainValue(module.getName(), module.getLocalisedTitle()));
			}
		}

		return result;
	}


	/**
	 * Add to the subject tag records from the object tag.
	 * 
	 * @param subject
	 * @param object
	 */
	public static void union(TagExtension subject, TagExtension object) throws Exception {
		if (subject != null && object != null) {
			// no dialect insensitive way to use SQL for creating new UUIDs for new records
			// add tagged items from object tag to subject tag
			// TagManager function deals with duplicates
			TagManager tm = EXT.getTagManager();
			try (AutoClosingIterable<Bean> i = tm.iterate(object.getBizId())) {
				for (Bean bean : i) {
					tm.tag(subject.getBizId(), bean);
				}
			}
			subject.setUploadTagged(Long.valueOf(subject.countDocument(subject.getUploadModuleName(), subject.getUploadDocumentName())));
			subject.setTotalTagged(Long.valueOf(subject.count()));
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
	public static void intersect(TagExtension subject, TagExtension object) throws Exception {
		if (subject != null && object != null) {
			Persistence pers = CORE.getPersistence();

			// unsecured SQL for performance
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
			
			subject.setUploadTagged(Long.valueOf(subject.countDocument(subject.getUploadModuleName(), subject.getUploadDocumentName())));
			subject.setTotalTagged(Long.valueOf(subject.count()));
		}
	}

	/**
	 * Deletes from the subject Tag items which are in the object Tag.
	 * 
	 * @param subject
	 * @param object
	 * @throws Exception
	 */
	public static void except(TagExtension subject, TagExtension object) throws Exception {
		if (subject != null && object != null) {
			TagManager tm = EXT.getTagManager();
			try (AutoClosingIterable<Bean> i = tm.iterate(object.getBizId())) {
				for (Bean bean : i) {
					// TagManager method handles if this bean was not tagged
					tm.untag(subject.getBizId(), bean);
				}
			}
			subject.setUploadTagged(Long.valueOf(subject.countDocument(subject.getUploadModuleName(), subject.getUploadDocumentName())));
			subject.setTotalTagged(Long.valueOf(subject.count()));
		}
	}

	/**
	 * Retrieve the items tagged which match the specified module and document
	 * 
	 * @param mailout
	 * @return
	 * @throws Exception
	 */
	public static List<Bean> getTaggedItemsForDocument(TagExtension tag, String moduleName, String documentName) throws Exception {
		
		List<Bean> beans = new ArrayList<>();
		if (moduleName != null && documentName != null) {
			Persistence pers = CORE.getPersistence();
			User user = pers.getUser();
			Customer customer = user.getCustomer();
			Module module = customer.getModule(moduleName);
			Document document = module.getDocument(customer, documentName);

			if (tag != null) {
				try (AutoClosingIterable<Bean> i = EXT.getTagManager().iterate(tag.getBizId())) {
					for (Bean bean : i) {
						if (bean != null && bean.getBizModule().equals(module.getName()) && bean.getBizDocument().equals(document.getName())) {
							// need to check that this is only done for documents of the selected type
							beans.add(bean);
						}
					}
				}
			}
		}

		return beans;
	}

	@Override
	public void preRerender(String source, TagExtension bean, WebContext webContext) throws Exception {
		
		switch(source) {
		case Tag.uploadModuleNamePropertyName:
			bean.setUploadDocumentName(null);
			//$FALL-THROUGH$
		case Tag.uploadDocumentNamePropertyName:
			bean.setAttributeName(null);
			bean.setDocumentCondition(null);
			bean.setUploadTagged(Long.valueOf(bean.countDocument(bean.getUploadModuleName(), bean.getUploadDocumentName())));			
			break;
		case Tag.actionModuleNamePropertyName:
			bean.setActionDocumentName(null);
			//$FALL-THROUGH$
		case Tag.actionDocumentNamePropertyName:
			bean.setDocumentAction(null);
			break;
		default:
			break;
		}
		super.preRerender(source, bean, webContext);
	}
	
	
}
