package modules.admin.Communication;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.app.admin.Communication.FormatType;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.ValidationException;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.DocumentPermissionScope;
import org.skyve.metadata.user.User;
import org.skyve.persistence.DocumentFilter;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;
import org.skyve.util.Util;
import org.skyve.web.WebContext;

import modules.admin.Jobs.JobsBizlet;
import modules.admin.domain.Communication;
import modules.admin.domain.CommunicationTemplate;
import modules.admin.domain.Tag;

public class CommunicationBizlet extends Bizlet<Communication> {
	public static final String SYSTEM_COMMUNICATION_JOB_NOTIFICATION = "SYSTEM Communication Job Notification";
	public static final String SYSTEM_COMMUNICATION_JOB_DEFAULT_SUBJECT = "Bulk Communication Job for '{description}' - Complete";
	public static final String SYSTEM_COMMUNICATION_JOB_DEFAULT_BODY = "The bulk communication job '{description}' for Tag '{tag.name}' is complete." + JobsBizlet.SYSTEM_JOB_NOTIFICATION_LINK_TO_JOBS;
	
	@Override
	public Communication newInstance(Communication communication) throws Exception {

		// set defaults
		Communication bean = communication;
		bean.setFormatType(FormatType.email);
		bean = setLinks(bean);

		return super.newInstance(bean);
	}

	@Override
	public Communication preExecute(ImplicitActionName actionName, Communication communication, Bean parentBean, WebContext webContext) throws Exception {
		Communication bean=  communication;
		
		if (ImplicitActionName.Edit.equals(actionName)) {
			bean = setLinks(bean);
		}
		return super.preExecute(actionName, bean, parentBean, webContext);
	}

	public static void checkForUnsavedData(Communication communication) throws Exception {
		if (!communication.originalValues().isEmpty()) {
			// find if any field except results
			for (String s : communication.originalValues().keySet()) {
				if (!Communication.resultsPropertyName.equals(s)) {
					throw new ValidationException(new Message("You have unsaved changes. The Job cannot be run until data is saved." + s));
				}
			}
		}
	}

	@Override
	public List<DomainValue> getDynamicDomainValues(String attributeName, Communication bean) throws Exception {

		Persistence pers = CORE.getPersistence();
		List<DomainValue> result = new ArrayList<>();

		Customer customer = pers.getUser().getCustomer();

		if (Communication.documentNamePropertyName.equals(attributeName)) {
			if (bean.getModuleName() != null) {
				Module module = customer.getModule(bean.getModuleName());
				for (String documentName : module.getDocumentRefs().keySet()) {
					Document document = module.getDocument(customer, documentName);
					result.add(new DomainValue(document.getName(), document.getLocalisedSingularAlias()));
				}
				result.sort(Comparator.comparing(DomainValue::getLocalisedDescription));
			}
		}

		return result;
	}

	@Override
	public List<DomainValue> getVariantDomainValues(String attributeName) throws Exception {

		List<DomainValue> result = new ArrayList<>();
		Persistence pers = CORE.getPersistence();

		Customer customer = pers.getUser().getCustomer();
		if (Communication.moduleNamePropertyName.equals(attributeName)) {
			for (Module module : customer.getModules()) {
				result.add(new DomainValue(module.getName(), module.getLocalisedTitle()));
			}
		}

		if (Communication.tagPropertyName.equals(attributeName)) {

			// look for OTHER tags
			DocumentQuery q = pers.newDocumentQuery(Tag.MODULE_NAME, Tag.DOCUMENT_NAME);
			q.addBoundOrdering(Tag.namePropertyName);

			List<Tag> tags = q.beanResults();
			for (Tag t : tags) {
				result.add(new DomainValue(t.getBizId(), t.getName()));
			}
		} else if (Communication.templatePropertyName.equals(attributeName)) {
			DocumentQuery q = pers.newDocumentQuery(CommunicationTemplate.MODULE_NAME, CommunicationTemplate.DOCUMENT_NAME);
			List<CommunicationTemplate> templates = q.beanResults();
			result.addAll(templates.stream()
					.map(t -> new DomainValue(t.getBizId(), t.getBizKey()))
					.collect(Collectors.toList()));
		}

		return result;
	}

	@Override
	public void preDelete(Communication bean) throws Exception {
		if (bean.isLocked()) {

			Persistence pers = CORE.getPersistence();
			User user = pers.getUser();
			Customer customer = user.getCustomer();
			Module module = customer.getModule(Communication.MODULE_NAME);
			Document document = module.getDocument(customer, Communication.DOCUMENT_NAME);

			StringBuilder sb = new StringBuilder(64);
			@SuppressWarnings("null")
			String su = document.getAttribute(Communication.systemUsePropertyName).getLocalisedDisplayName();
			sb.append(su).append(' ').append(document.getLocalisedPluralAlias());
			sb.append(" may not be deleted unless ");
			sb.append(su).append(" is set to FALSE.");

			throw new ValidationException(new Message(sb.toString()));
		}
		super.preDelete(bean);
	}

	@Override
	public void preRerender(String source, Communication bean, WebContext webContext) throws Exception {

		if (Communication.moduleNamePropertyName.equals(source)) {
			bean.setDocumentName(null);
		}

		super.preRerender(source, bean, webContext);
	}

	/**
	 * anonymously check whether a communication exists for a customer
	 * 
	 * Used in UnsubscribeView
	 * 
	 * @param p
	 * @param bizCustomer
	 * @param communicationId
	 * @return
	 */
	@SuppressWarnings("boxing")
	public static boolean anonymouslyCommunicationExists(Persistence persistence, String bizCustomer, String communicationId) {
		// temporarily elevate user to be able to see Communication records in case they don't usually have access
		return persistence.withDocumentPermissionScopes(DocumentPermissionScope.customer, p -> {
			DocumentQuery q = p.newDocumentQuery(Communication.MODULE_NAME, Communication.DOCUMENT_NAME);
			q.addBoundProjection(Bean.DOCUMENT_ID);
			DocumentFilter f = q.getFilter();
			f.addEquals(Bean.CUSTOMER_NAME, bizCustomer);
			f.addEquals(Bean.DOCUMENT_ID, communicationId);
			return q.scalarResult(String.class) != null;
		});
	}

	public static Communication setLinks(Communication communication) {
		Communication bean = communication;

		// construct the unsubscribe URL
		StringBuilder url = new StringBuilder(256);
		url.append(Util.getSkyveContextUrl());
		url.append("/");
		url.append("unsubscribe.xhtml?c=").append(bean.getBizCustomer());
		url.append("&i=").append(bean.getBizId());
		url.append("&r=").append(bean.getSendTo());
		bean.setUnsubscribeUrl(url.toString());

		return bean;
	}

}