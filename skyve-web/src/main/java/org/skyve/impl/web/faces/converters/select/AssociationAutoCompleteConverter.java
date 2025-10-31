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

public class AssociationAutoCompleteConverter implements Converter<Object> {
    @Override
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

		            Bean bean = null;
		            WebContext webContext = null;
					UIViewRoot root = context.getViewRoot();
					if (root != null) {
						String managedBeanName = (String) root.getAttributes().get(FacesUtil.MANAGED_BEAN_NAME_KEY);
						if (managedBeanName != null) {
							FacesView view = (FacesView) FacesUtil.getNamed(managedBeanName);
							if (view != null) {
								BeanMapAdapter adapter = view.getCurrentBean();
								if (adapter != null) {
									bean = adapter.getBean();
								}
								webContext = view.getWebContext();
							}
						}
					}
					
		            Customer c = CORE.getCustomer();
		            Module m  = c.getModule(moduleName);
		            Document d = m.getDocument(c, documentName);
		            return WebUtil.findReferencedBean(d, bizId, CORE.getPersistence(), bean, webContext);
				}
			}.execute();
        }

        return result;
    }

    @Override
    public String getAsString(FacesContext context, UIComponent component, Object value) {
    	String result = "";
    	
    	Bean bean = null;
		if (value instanceof BeanMapAdapter) {
			bean = ((BeanMapAdapter) value).getBean();
		}
		else if (value instanceof Bean) {
        	bean = (Bean) value;
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
