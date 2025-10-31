package org.skyve.impl.web.faces.pipeline;

import org.primefaces.component.column.Column;
import org.primefaces.component.message.Message;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.view.HorizontalAlignment;
import org.skyve.impl.metadata.view.LayoutUtil;
import org.skyve.impl.web.faces.FacesUtil;
import org.skyve.impl.web.faces.views.FacesView;
import org.skyve.metadata.view.TextOutput.Sanitisation;
import org.skyve.web.UserAgentType;

import jakarta.el.ELContext;
import jakarta.el.ExpressionFactory;
import jakarta.el.ValueExpression;
import jakarta.faces.application.Application;
import jakarta.faces.component.UIComponent;
import jakarta.faces.component.html.HtmlPanelGroup;
import jakarta.faces.context.FacesContext;

public abstract class AbstractFacesBuilder {
	protected static final Integer ONE_HUNDRED = Integer.valueOf(100);

	protected static final String PROCESS_KEY = "process";
	protected static final String UPDATE_KEY = "update";

	// NOTE:- Any of this protected state needs to be set in the ComponentBuilderChain and LayoutBuilderChain.
	//			Otherwise the protected utility methods in this class that rely on this state wont work.
	
	protected FacesContext fc = FacesContext.getCurrentInstance();
	protected Application a = fc.getApplication();
	protected ExpressionFactory ef = a.getExpressionFactory();
	protected ELContext elc = fc.getELContext();
	protected String managedBeanName = "skyve";
	protected FacesView managedBean;
	protected String process = "@form";
	protected String update = "@(form)";
	protected UserAgentType userAgentType;
	
	public void setManagedBeanName(String managedBeanName) {
		if (managedBeanName != null) {
			this.managedBeanName = managedBeanName;
		}
		// Do nothing if this is being executed through SAIL
		if (FacesUtil.isRealFacesContext()) {
			managedBean = (FacesView) FacesUtil.getNamed(managedBeanName);
		}
	}
	
	public void setSAILManagedBean(FacesView managedBean) {
		this.managedBean = managedBean;
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
	
	protected void setDisabled(UIComponent component, String disabledConditionName, String formDisabledConditionName) {
		if (disabledConditionName != null) {
			if (formDisabledConditionName == null) {
				component.setValueExpression("disabled", createValueExpressionFromCondition(disabledConditionName, null));
			}
			else {
				component.setValueExpression("disabled", createOredValueExpressionFromConditions(new String[] {disabledConditionName, formDisabledConditionName}));
			}
		}
		else if (formDisabledConditionName != null) {
			component.setValueExpression("disabled", createValueExpressionFromCondition(formDisabledConditionName, null));
		}
	}

	protected void setInvisible(UIComponent component, String invisibleConditionName, String extraELToAnd) {
		if (invisibleConditionName != null) {
			String visible = BindUtil.negateCondition(invisibleConditionName);
			component.setValueExpression("rendered", createValueExpressionFromCondition(visible, extraELToAnd));
		}
	}

	protected void setInvisible(UIComponent component, String dataWidgetVar, String invisibleConditionName, String extraELToAnd) {
		if (invisibleConditionName != null) {
			final String visible = BindUtil.negateCondition(invisibleConditionName);
			component.setValueExpression("rendered",  createValueExpressionFromFragment(dataWidgetVar,
																							true,
																							visible,
																							false,
																							extraELToAnd,
																							Boolean.class,
																							false,
																							Sanitisation.none));
		}
	}

	protected void setTextAlign(UIComponent component, HorizontalAlignment textAlignment) {
		if (textAlignment != null) {
			component.setValueExpression("styleClass", ef.createValueExpression(textAlignment.toTextAlignmentString(), String.class));
		}
	}
	
	protected final void setSizeAndTextAlignStyle(UIComponent component,
													String existingStyle, 
													Integer pixelWidth, 
													Integer responsiveWidth,
													Integer percentageWidth,
													Integer pixelHeight, 
													Integer percentageHeight, 
													Integer defaultPercentageWidth) {
		setSizeAndTextAlignStyle(component, existingStyle, pixelWidth, responsiveWidth, percentageWidth, pixelHeight, percentageHeight, defaultPercentageWidth, null, null, null);
	}
	
	protected void setSizeAndTextAlignStyle(UIComponent component,
												String existingStyle, 
												Integer pixelWidth, 
												Integer responsiveWidth,
												Integer percentageWidth,
												Integer pixelHeight, 
												Integer percentageHeight, 
												Integer defaultPercentageWidth,
												HorizontalAlignment textAlign,
												String specialTextAlignStyleAttributeName, // if null, "style" is used.
												String rightPaddingIfNecessary) {
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
		if (textAlign != null) {
			if (specialTextAlignStyleAttributeName == null) {
				if (style.length() > 0) {
					style.append(';');
				}
				if ((rightPaddingIfNecessary != null) && (textAlign == HorizontalAlignment.right)) {
					style.append("padding-right:").append(rightPaddingIfNecessary).append(';');
				}
				style.append("text-align:").append(textAlign.toTextAlignmentString());
			}
			else {
				StringBuilder textAlignStyle = new StringBuilder(32);
				if ((rightPaddingIfNecessary != null) && (textAlign == HorizontalAlignment.right)) {
					textAlignStyle.append("padding-right:").append(rightPaddingIfNecessary).append(';');
				}
				textAlignStyle.append("text-align:").append(textAlign.toTextAlignmentString());
				component.setValueExpression(specialTextAlignStyleAttributeName, ef.createValueExpression(textAlignStyle, String.class));
			}
		}
		component.setValueExpression("style", ef.createValueExpression(style.toString(), String.class));
	}

	protected ValueExpression createValueExpressionFromFragment(String fragment, 
																	boolean map,
																	String extraELConditionToAnd, 
																	Class<?> typeReturned,
																	boolean escape,
																	Sanitisation sanitise) {
		return createValueExpressionFromFragment(managedBeanName + ".currentBean",
													false,
													fragment, 
													map, 
													extraELConditionToAnd,
													typeReturned,
													escape,
													sanitise);
	}

	protected ValueExpression createValueExpressionFromFragment(String expressionPrefix, 
																	boolean dataWidgetVar,
																	String fragment, 
																	boolean map,
																	String extraELConditionToAnd, 
																	Class<?> typeReturned,
																	boolean escape,
																	Sanitisation sanitise) {
		// default sanitisation is relaxed, for widgets that have no sanitisation set
		Sanitisation finalSanitise = (sanitise == null) ? Sanitisation.relaxed : sanitise;
		boolean escapeOrSanitise = escape || (! Sanitisation.none.equals(finalSanitise));
		StringBuilder sb = new StringBuilder(64);
		sb.append("#{");
		if (expressionPrefix != null) {
			sb.append(dataWidgetVar ? BindUtil.sanitiseBinding(expressionPrefix) : expressionPrefix);
			sb.append(escapeOrSanitise ? ".get('" : (map ? "['" : "."));
		}
		sb.append(fragment);
		if (expressionPrefix != null) {
			if (escapeOrSanitise) {
				sb.append("',").append(escape).append(",'").append(finalSanitise).append("')");
			}
			else if (map) {
				sb.append("']");
			}
		}
		if (extraELConditionToAnd != null) {
			sb.append(" and ").append(extraELConditionToAnd);
		}
		sb.append('}');

		return ef.createValueExpression(elc, sb.toString(), typeReturned);
	}

	protected ValueExpression createValueExpressionFromCondition(String condition, String extraELConditionToAnd) {
		if (String.valueOf(false).equals(condition)) {
			return ef.createValueExpression(condition, Boolean.class);
		}
		else if (String.valueOf(true).equals(condition)) {
			if (extraELConditionToAnd == null) {
				return ef.createValueExpression(condition, Boolean.class);
			}

			return createValueExpressionFromFragment(null, false, extraELConditionToAnd, false, null, Boolean.class, false, Sanitisation.none);
		}

		return createValueExpressionFromFragment(condition, true, extraELConditionToAnd, Boolean.class, false, Sanitisation.none);
	}
	
	protected String createOredValueExpressionFragmentFromConditions(String[] conditions) {
		StringBuilder result = new StringBuilder(64);
		
		if ((conditions != null) && (conditions.length > 0)) {
			for (String condition : conditions) {
				if (String.valueOf(true).equals(condition) || String.valueOf(false).equals(condition)) {
					result.append(condition);
				}
				else {
					result.append(managedBeanName).append(".currentBean['").append(condition).append("']");
				}
				result.append(" or ");
			}
			result.setLength(result.length() - 4); // remove last or
		}
		
		return result.toString();
	}
	
	protected ValueExpression createOredValueExpressionFromConditions(String[] conditions) {
		if (conditions == null) {
			return null;
		}
		if (conditions.length == 1) {
			return createValueExpressionFromCondition(conditions[0], null);
		}
		if (conditions.length > 0) {
			return createValueExpressionFromFragment(null, 
														false, 
														createOredValueExpressionFragmentFromConditions(conditions), 
														false, 
														null, 
														Boolean.class,
														false,
														Sanitisation.none);
		}
		return null;
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
								int colspan, 
								int rowspan) {
		Column result = (Column) a.createComponent(Column.COMPONENT_TYPE);
		setInvisible(result, invisible, null);
		setId(result, null);
		if (colspan > 1) {
			result.setColspan(colspan);
		}
		if (rowspan > 1) {
			result.setRowspan(rowspan);
		}

		String existingStyle = noWrap ? 
								(top ? "white-space:nowrap;vertical-align:top !important;" : "white-space:nowrap;") :
								(top ? "vertical-align:top !important;" : null);
		setSizeAndTextAlignStyle(result, existingStyle, pixelWidth, responsiveWidth, percentageWidth, null, null, null);

		return result;
	}
	
	protected Message message(String forId) {
		Message message = (Message) a.createComponent(Message.COMPONENT_TYPE);
		setId(message, null);
		message.setFor(forId);
		message.setShowDetail(true);
		message.setShowSummary(false);
		message.setDisplay("icon");
		message.setEscape(false);

		return message;
	}
}
