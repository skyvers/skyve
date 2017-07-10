package org.skyve.impl.web.faces.pipeline;

import javax.el.ELContext;
import javax.el.ExpressionFactory;
import javax.el.ValueExpression;
import javax.faces.application.Application;
import javax.faces.component.UIComponent;
import javax.faces.component.html.HtmlPanelGroup;
import javax.faces.context.FacesContext;

import org.primefaces.component.column.Column;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.view.LayoutUtil;
import org.skyve.impl.web.UserAgent.UserAgentType;
import org.skyve.impl.web.faces.FacesUtil;
import org.skyve.impl.web.faces.beans.FacesView;

public abstract class AbstractFacesBuilder {
	protected static final Integer ONE_HUNDRED = Integer.valueOf(100);
	protected static final Integer NINETY_EIGHT = Integer.valueOf(98);
	protected static final Integer NINETY_FIVE = Integer.valueOf(95);

	protected FacesContext fc = FacesContext.getCurrentInstance();
	protected Application a = fc.getApplication();
	protected ExpressionFactory ef = a.getExpressionFactory();
	protected ELContext elc = fc.getELContext();
	protected String managedBeanName = "skyve";
	protected FacesView<?> managedBean;
	protected String process = "@form";
	protected String update = "@(form)";
	protected UserAgentType userAgentType;
	
	public void setManagedBeanName(String managedBeanName) {
		if (managedBeanName != null) {
			this.managedBeanName = managedBeanName;
		}
		managedBean = FacesUtil.getManagedBean(managedBeanName);
	}
	public void setProcess(String process) {
		if (process != null) {
			this.process = process;
		}
	}
	public void setUpdate(String update) {
		if (update != null) {
			this.update = update;
		}
	}
	public void setUserAgentType(UserAgentType userAgentType) {
		this.userAgentType = userAgentType;
	}
	
	protected void setId(UIComponent component, String widgetId) {
		component.setId((widgetId == null) ? managedBean.nextId() : widgetId);
	}
	
	protected void setDisabled(UIComponent component, String disabledConditionName) {
		if (disabledConditionName != null) {
			component.setValueExpression("disabled", createValueExpressionFromCondition(disabledConditionName, null));
		}
	}

	protected void setInvisible(UIComponent component, String invisibleConditionName, String extraELToAppend) {
		if (invisibleConditionName != null) {
			String visible = BindUtil.negateCondition(invisibleConditionName);
			component.setValueExpression("rendered", createValueExpressionFromCondition(visible, extraELToAppend));
		}
	}

	protected void setSize(UIComponent component, 
							String existingStyle, 
							Integer pixelWidth, 
							Integer responsiveWidth,
							Integer percentageWidth,
							Integer pixelHeight, 
							Integer percentageHeight, 
							Integer defaultPercentageWidth) {
		StringBuilder style = new StringBuilder(64);
		boolean noWidth = true;
		if (existingStyle != null) {
			style.append(existingStyle);
		}
		if (pixelWidth != null) {
			noWidth = false;
			style.append("width:").append(pixelWidth).append("px");
		} 
		else if (responsiveWidth != null) {
			noWidth = false;
			style.append("width:");
			style.append(LayoutUtil.responsiveWidthToPercentageWidth(responsiveWidth.doubleValue()));
			style.append('%');
		}
		else if (percentageWidth != null) {
			noWidth = false;
			style.append("width:").append(percentageWidth).append('%');
		}
		if (noWidth && (defaultPercentageWidth != null)) {
			style.append("width:").append(defaultPercentageWidth).append('%');
		}
		if (pixelHeight != null) {
			if (style.length() > 0) {
				style.append(';');
			}
			style.append("height:").append(pixelHeight).append("px");
		}
		else if (percentageHeight != null) {
			if (style.length() > 0) {
				style.append(';');
			}
			style.append("height:").append(percentageHeight).append("%");
		}
		component.setValueExpression("style", ef.createValueExpression(style.toString(), String.class));
	}

	protected ValueExpression createValueExpressionFromBinding(String binding, 
																boolean map,
																String extraELConditionToAppend, 
																Class<?> typeReturned) {
		StringBuilder expressionPrefix = new StringBuilder(64);
		expressionPrefix.append(managedBeanName).append(".currentBean");
		String fullBinding = binding;

		return createValueExpressionFromBinding(expressionPrefix.toString(), 
													false,
													fullBinding, 
													map, 
													extraELConditionToAppend,
													typeReturned);
	}

	protected ValueExpression createValueExpressionFromBinding(String expressionPrefix, 
																boolean listVar,
																String binding, 
																boolean map,
																String extraELConditionToAppend, 
																Class<?> typeReturned) {
		StringBuilder sb = new StringBuilder(64);
		sb.append("#{");
		if (expressionPrefix != null) {
			sb.append(listVar ? expressionPrefix.replace('.', '_') : expressionPrefix);
			sb.append(map ? "['" : ".");
		}
		sb.append(binding);
		if (map && (expressionPrefix != null)) {
			sb.append("']");
		}
		if (extraELConditionToAppend != null) {
			sb.append(" and ").append(extraELConditionToAppend);
		}
		sb.append('}');

		return ef.createValueExpression(elc, sb.toString(), typeReturned);
	}

	protected ValueExpression createValueExpressionFromCondition(String condition, String extraELConditionToAppend) {
		if ("false".equals(condition)) {
			return ef.createValueExpression(condition, Boolean.class);
		}
		else if ("true".equals(condition)) {
			if (extraELConditionToAppend == null) {
				return ef.createValueExpression(condition, Boolean.class);
			}

			return createValueExpressionFromBinding(null, false, extraELConditionToAppend, false, null, Boolean.class);
		}

		return createValueExpressionFromBinding(condition, true, extraELConditionToAppend, Boolean.class);
	}
	
	protected HtmlPanelGroup panelGroup(boolean nowrap, 
											boolean middle, 
											boolean blockLayout,
											String invisibleConditionName,
											String widgetId) {
		HtmlPanelGroup result = (HtmlPanelGroup) a.createComponent(HtmlPanelGroup.COMPONENT_TYPE);
		StringBuilder style = new StringBuilder(32);
		if (nowrap) {
			style.append("white-space:nowrap;");
		}
		if (middle) {
			style.append("vertical-align:middle;");
		}
		if (style.length() > 0) {
			result.setStyle(style.toString());
		}
		setInvisible(result, invisibleConditionName, null);
		setId(result, widgetId);
		if (blockLayout) {
			result.setLayout("block");
		}
		return result;
	}
	
	protected Column column(String invisible, 
								boolean noWrap, 
								boolean top, 
								Integer pixelWidth, 
								Integer responsiveWidth,
								Integer percentageWidth,
								Integer colspan, 
								Integer rowspan) {
		Column result = (Column) a.createComponent(Column.COMPONENT_TYPE);
		setInvisible(result, invisible, null);
		setId(result, null);
		if (colspan != null) {
			result.setColspan(colspan.intValue());
		}
		if (rowspan != null) {
			result.setRowspan(rowspan.intValue());
		}

		String existingStyle = noWrap ? 
								(top ? "white-space:nowrap;vertical-align:top !important;" : "white-space:nowrap;") :
								(top ? "vertical-align:top !important;" : null);
		setSize(result, existingStyle, pixelWidth, responsiveWidth, percentageWidth, null, null, null);

		return result;
	}
}
