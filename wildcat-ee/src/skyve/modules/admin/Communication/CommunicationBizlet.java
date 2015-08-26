package modules.admin.Communication;

import java.util.ArrayList;
import java.util.List;

import modules.admin.domain.Communication;
import modules.admin.domain.Communication.FormatType;
import modules.admin.domain.Tag;

import org.skyve.CORE;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.ValidationException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;

public class CommunicationBizlet extends Bizlet<Communication> {

	/**
	 * 
	 */
	private static final long serialVersionUID = -7404508611264793559L;

	@Override
	public Communication newInstance(Communication bean) throws Exception {

		// set defaults
		bean.setFormatType(FormatType.email);

		return super.newInstance(bean);
	}

	@Override
	public void validate(Communication bean, ValidationException e) throws Exception {
		Persistence pers = CORE.getPersistence();
		User user = pers.getUser();
		Customer customer = user.getCustomer();
		Module module = customer.getModule(Communication.MODULE_NAME);
		Document document = module.getDocument(customer, Communication.DOCUMENT_NAME);
		
		if(!Boolean.TRUE.equals(bean.getSystem()) && bean.getSendFrom()==null){
			StringBuilder sb = new StringBuilder(128);
			sb.append("You must supply a ").append(document.getAttribute(Communication.sendFromPropertyName).getDisplayName());
			sb.append(" unless ").append(document.getAttribute(Communication.systemPropertyName).getDisplayName());
			sb.append(" has been set to TRUE");
			e.getMessages().add(new Message(Communication.sendFromPropertyName, sb.toString()));
		}
		super.validate(bean, e);
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
					result.add(new DomainValue(document.getName(), document.getDescription()));
				}
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
				result.add(new DomainValue(module.getName(), module.getTitle()));
			}
		}

		if (Communication.tagPropertyName.equals(attributeName)) {

			// look for OTHER tags
			DocumentQuery q = pers.newDocumentQuery(Tag.MODULE_NAME, Tag.DOCUMENT_NAME);
			q.addOrdering(Tag.namePropertyName);

			List<Tag> tags = q.beanResults();
			for (Tag t : tags) {
				result.add(new DomainValue(t.getBizId(), t.getName()));
			}
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
			sb.append(document.getAttribute(Communication.systemPropertyName).getDisplayName());
			sb.append(' ').append(document.getPluralAlias());
			sb.append(" may not be deleted unless ");
			sb.append(document.getAttribute(Communication.systemPropertyName).getDisplayName());
			sb.append(" is set to FALSE.");

			throw new ValidationException(new Message(sb.toString()));
		}
		super.preDelete(bean);
	}
}