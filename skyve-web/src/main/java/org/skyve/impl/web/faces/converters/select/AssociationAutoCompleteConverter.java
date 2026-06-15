package org.skyve.impl.web.faces.converters.select;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.WebUtil;
import org.skyve.impl.web.faces.FacesAction;
import org.skyve.impl.web.faces.FacesUtil;
import org.skyve.impl.web.faces.models.BeanMapAdapter;
import org.skyve.impl.web.faces.views.FacesView;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.web.WebContext;

import jakarta.faces.component.UIComponent;
import jakarta.faces.component.UIViewRoot;
import jakarta.faces.context.FacesContext;
import jakarta.faces.convert.Converter;

/**
 * Converts JSF values between formatted UI strings and Skyve domain representations for this format.
 */
public class AssociationAutoCompleteConverter implements Converter<Object> {
	/**
	 * Resolves an association identifier string back to a referenced bean.
	 *
	 * @param context the active JSF context
	 * @param component the component requesting conversion
	 * @param value the submitted association token
	 * @return the resolved referenced bean, or {@code null} when the submitted value is empty
	 */
    @Override
    @SuppressWarnings("java:S3776") // Complexity OK
    public Object getAsObject(FacesContext context, UIComponent component, String value) {
    	Bean result = null;
    	
    	final String processedValue = UtilImpl.processStringValue(value);
    	if (processedValue != null) {
            result = new FacesAction<Bean>() {
				@Override
				public Bean callback() throws Exception {
		            int pos = processedValue.indexOf('.');
		            String moduleName = processedValue.substring(0, pos);
		            String documentName = processedValue.substring(pos + 1);
		            pos = documentName.indexOf('.');
		            String bizId = documentName.substring(pos + 1);
		            documentName = documentName.substring(0, pos);

					UIViewRoot root = context.getViewRoot();
					if (root != null) {
						String managedBeanName = (String) root.getAttributes().get(FacesUtil.MANAGED_BEAN_NAME_KEY);
						if (managedBeanName != null) {
							FacesView view = (FacesView) FacesUtil.getNamed(managedBeanName);
							if (view != null) {
								BeanMapAdapter adapter = view.getCurrentBean();
								Bean bean = null;
								if (adapter != null) {
									bean = adapter.getBean();
								}
								
								WebContext webContext = view.getWebContext();
					            Customer c = CORE.getCustomer();
					            Module m  = c.getModule(moduleName);
					            Document d = m.getDocument(c, documentName);
					        	return WebUtil.findReferencedBean(d, bizId, CORE.getPersistence(), bean, webContext);
							}
						}
					}
					return null;
				}
			}.execute();
        }

        return result;
    }

	/**
	 * Formats a referenced bean as the module.document.bizId token used by this converter.
	 *
	 * @param context the active JSF context
	 * @param component the component requesting conversion
	 * @param value the referenced bean or bean adapter to format
	 * @return the encoded association token, or an empty string when no bean is supplied
	 */
    @Override
    public String getAsString(FacesContext context, UIComponent component, Object value) {
    	String result = "";
    	
    	Bean bean = null;
		if (value instanceof BeanMapAdapter beanMapAdapter) {
			bean = beanMapAdapter.getBean();
		}
		else if (value instanceof Bean b) {
        	bean = b;
        }

    	if (bean != null) {
            StringBuilder sb = new StringBuilder(64);
            sb.append(bean.getBizModule()).append('.');
            sb.append(bean.getBizDocument()).append('.').append(bean.getBizId());
            result = sb.toString();
    	}

        return result;
    }
}
