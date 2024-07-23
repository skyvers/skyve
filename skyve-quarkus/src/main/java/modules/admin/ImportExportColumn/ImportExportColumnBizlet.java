package modules.admin.ImportExportColumn;

import java.util.ArrayList;
import java.util.List;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.ValidationException;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.persistence.Persistence;
import org.skyve.util.Binder;
import org.skyve.util.Binder.TargetMetaData;
import org.skyve.web.WebContext;

import modules.admin.domain.ImportExport.Mode;
import modules.admin.domain.ImportExportColumn;

public class ImportExportColumnBizlet extends Bizlet<ImportExportColumn> {

	public static final String EXPRESSION = "expression...";

	private List<DomainValue> bindings = null;

	@Override
	public List<DomainValue> getDynamicDomainValues(String attributeName, ImportExportColumn bean) throws Exception {

		if (ImportExportColumn.bindingNamePropertyName.equals(attributeName)) {
			if (bindings == null) {
				bindings = new ArrayList<>();
			}

			if (bean.getParent() != null && bean.getParent().getModuleName() != null && bean.getParent().getDocumentName() != null && bindings.isEmpty()) {

				Persistence pers = CORE.getPersistence();
				User user = pers.getUser();
				Customer customer = user.getCustomer();
				Module module = customer.getModule(bean.getParent().getModuleName());
				Document document = module.getDocument(customer, bean.getParent().getDocumentName());

				for (Attribute a : document.getAllAttributes(customer)) {

					// exclude unimplemented types - some of these can be handled later
					if (!AttributeType.collection.equals(a.getAttributeType())
							&& !AttributeType.content.equals(a.getAttributeType())
							&& !AttributeType.image.equals(a.getAttributeType())
							&& !AttributeType.geometry.equals(a.getAttributeType())
							&& !AttributeType.inverseMany.equals(a.getAttributeType())
							&& !AttributeType.inverseOne.equals(a.getAttributeType())) {

						// also exclude non persistent fields
						if (a.isPersistent()) {
							if(AttributeType.association.equals(a.getAttributeType())) {
//								bindings.add(new DomainValue(a.getName() + Bean.BIZ_KEY, a.getDisplayName()));
								bindings.add(new DomainValue(a.getName(), a.getLocalisedDisplayName()));
							} else {
								bindings.add(new DomainValue(a.getName(), a.getLocalisedDisplayName()));
							}
						}
					}
				}

				bindings.add(new DomainValue(EXPRESSION));
			}

			return bindings;
		}
		return super.getDynamicDomainValues(attributeName, bean);
	}

	@Override
	public ImportExportColumn preExecute(ImplicitActionName actionName, ImportExportColumn bean, Bean parentBean, WebContext webContext) throws Exception {

		if (ImplicitActionName.OK.equals(actionName) || ImplicitActionName.Save.equals(actionName) || ImplicitActionName.ZoomOut.equals(actionName)) {

			Persistence pers = CORE.getPersistence();
			Customer customer = pers.getUser().getCustomer();
			Module module = customer.getModule(bean.getParent().getModuleName());
			Document document = module.getDocument(customer, bean.getParent().getDocumentName());

			String resolvedBinding = bean.getBindingName();
			if (bean.isShowExpression()) {
				resolvedBinding = bean.getBindingExpression();
			}

			if (resolvedBinding != null && resolvedBinding.startsWith("{") && resolvedBinding.endsWith("}")) {
				resolvedBinding = bean.getBindingExpression().substring(1, bean.getBindingExpression().length() - 1);
			}

			if (resolvedBinding != null) {
				// evaluate whether this is a valid compound binding or a binding expression
				try {

					// can we resolve the binding?
					TargetMetaData tm = Binder.getMetaDataForBinding(customer, module, document, resolvedBinding);
					@SuppressWarnings("unused")
					Attribute attr = tm.getAttribute();

				} catch (@SuppressWarnings("unused") Exception e) {

					if (Mode.importData.equals(bean.getParent().getMode())) {

						StringBuilder sb = new StringBuilder(64);
						sb.append("The binding '").append(resolvedBinding).append("' is invalid or can't be processed");

						throw new ValidationException(new Message(sb.toString()));

					}

					// for export Data compound Expressions are allowed
					try {
						// validate the compound Expression
						Bean b = document.newInstance(CORE.getUser());
						@SuppressWarnings("unused")
						String attempt = Binder.formatMessage(bean.getBindingExpression(), b);

					} catch (@SuppressWarnings("unused") Exception e2) {
						StringBuilder sb = new StringBuilder(64);
						sb.append("The expression '").append(bean.getBindingExpression()).append("' is invalid or can't be processed");

						throw new ValidationException(new Message(sb.toString()));
					}

				}
			}

		}

		return super.preExecute(actionName, bean, parentBean, webContext);
	}

}
