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

import modules.admin.ImportExport.ImportExportUtil;
import modules.admin.domain.ImportExport.Mode;
import modules.admin.domain.ImportExportColumn;

/**
 * Provides binding/domain validation logic for import/export column configuration rows.
 */
public class ImportExportColumnBizlet extends Bizlet<ImportExportColumn> {
	private List<DomainValue> bindings = null;

	/**
	 * Returns dynamic binding choices for the selected parent document.
	 *
	 * @param attributeName
	 *        the attribute requesting dynamic values
	 * @param bean
	 *        the current import/export column bean
	 * @return available binding domain values for {@link ImportExportColumn#bindingNamePropertyName}, otherwise superclass values
	 * @throws Exception
	 *         if metadata inspection fails
	 */
	@Override
	public List<DomainValue> getDynamicDomainValues(String attributeName, ImportExportColumn bean) throws Exception {

		// The dynamic domain logic for bindingName can be used when inline datagrids are fixed for both PF and SC
		if (ImportExportColumn.bindingNamePropertyName.equals(attributeName)) {
			if (bindings == null) {
				bindings = new ArrayList<>();
			}

			if (bean.getParent() != null && bean.getParent().getModuleName() != null && bean.getParent().getDocumentName() != null
					&& bindings.isEmpty()) {

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
							&& !AttributeType.inverseOne.equals(a.getAttributeType())
							// also exclude non persistent fields
							&& a.isPersistent()) {
						bindings.add(new DomainValue(a.getName(), a.getLocalisedDisplayName()));
					}
				}

				bindings.add(new DomainValue(ImportExportUtil.EXPRESSION));
			}

			return bindings;
		}
		return super.getDynamicDomainValues(attributeName, bean);
	}

	/**
	 * Returns completion candidates for binding names.
	 *
	 * @param attributeName
	 *        the attribute requesting completions
	 * @param value
	 *        current user-entered text
	 * @param bean
	 *        the current import/export column bean
	 * @return available binding completions for {@link ImportExportColumn#bindingNamePropertyName}, otherwise superclass values
	 * @throws Exception
	 *         if metadata inspection fails
	 */
	@Override
	public List<String> complete(String attributeName, String value, ImportExportColumn bean) throws Exception {
		if (ImportExportColumn.bindingNamePropertyName.equals(attributeName)) {
			List<String> bindingsList = new ArrayList<>();

			if (bean.getParent() != null && bean.getParent().getModuleName() != null && bean.getParent().getDocumentName() != null
					&& bindingsList.isEmpty()) {

				Customer customer = CORE.getCustomer();
				Module module = customer.getModule(bean.getParent().getModuleName());
				Document document = module.getDocument(customer, bean.getParent().getDocumentName());

				for (Attribute a : document.getAllAttributes(customer)) {

					// exclude unimplemented types - some of these can be handled later
					if (!AttributeType.collection.equals(a.getAttributeType())
							&& !AttributeType.content.equals(a.getAttributeType())
							&& !AttributeType.image.equals(a.getAttributeType())
							&& !AttributeType.geometry.equals(a.getAttributeType())
							&& !AttributeType.inverseMany.equals(a.getAttributeType())
							&& !AttributeType.inverseOne.equals(a.getAttributeType())
							// also exclude non persistent fields
							&& a.isPersistent()) {
						bindingsList.add(a.getName());
					}
				}

				bindingsList.add(ImportExportUtil.EXPRESSION);
			}

			return bindingsList;
		}
		return super.complete(attributeName, value, bean);
	}

	/**
	 * Validates binding or expression syntax before saving the inline row.
	 *
	 * @param actionName
	 *        the pending implicit action
	 * @param bean
	 *        the import/export column being validated
	 * @param parentBean
	 *        the parent bean from the conversation
	 * @param webContext
	 *        the current web context
	 * @return the bean passed to the superclass pre-execute pipeline
	 * @throws Exception
	 *         if validation fails
	 */
	@Override
	public ImportExportColumn preExecute(ImplicitActionName actionName, ImportExportColumn bean, Bean parentBean,
			WebContext webContext) throws Exception {

		if (ImplicitActionName.OK.equals(actionName) || ImplicitActionName.Save.equals(actionName)
				|| ImplicitActionName.ZoomOut.equals(actionName)) {

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
						sb.append("The expression '")
								.append(bean.getBindingExpression())
								.append("' is invalid or can't be processed");

						throw new ValidationException(new Message(sb.toString()));
					}

				}
			}

		}

		return super.preExecute(actionName, bean, parentBean, webContext);
	}
}
