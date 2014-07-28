package org.skyve.wildcat.web.faces.converters.select;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.convert.Converter;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.wildcat.util.UtilImpl;
import org.skyve.wildcat.web.faces.BeanMapAdapter;
import org.skyve.wildcat.web.faces.FacesAction;

public class AssociationAutoCompleteConverter implements Converter {
    @Override
    public Object getAsObject(FacesContext context, UIComponent component, String value) {
    	Object result = null;
    	
    	final String processedValue = UtilImpl.processStringValue(value);
    	if (processedValue != null) {
            result = new FacesAction<Object>() {
				@Override
				public Object callback() throws Exception {
		            int pos = processedValue.indexOf('.');
		            String moduleName = processedValue.substring(0, pos);
		            String documentName = processedValue.substring(pos + 1);
		            pos = documentName.indexOf('.');
		            String bizId = documentName.substring(pos + 1);
		            documentName = documentName.substring(0, pos);

		            return CORE.getPersistence().retrieve(moduleName, documentName, bizId, false);
				}
			}.execute();
        }

        return result;
    }

    @Override
    public String getAsString(FacesContext context, UIComponent component, Object value) {
    	String result = null;
    	
    	Bean bean = null;
		if (value instanceof BeanMapAdapter<?>) {
			bean = ((BeanMapAdapter<?>) value).getBean();
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
