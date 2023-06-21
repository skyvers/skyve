package org.skyve.impl.generate.client;

import org.skyve.impl.metadata.view.HorizontalAlignment;
import org.skyve.impl.metadata.view.LayoutUtil;
import org.skyve.web.UserAgentType;

public abstract class AbstractRenderer {
	protected static final Integer ONE_HUNDRED = Integer.valueOf(100);
	protected static final Integer NINETY_EIGHT = Integer.valueOf(98);
	protected static final Integer NINETY_FIVE = Integer.valueOf(95);

	// NOTE:- Any of this protected state needs to be set in the ComponentBuilderChain and LayoutBuilderChain.
	//			Otherwise the protected utility methods in this class that rely on this state wont work.
	
	protected UserAgentType userAgentType;
	
	public void setUserAgentType(UserAgentType userAgentType) {
		this.userAgentType = userAgentType;
	}
	
	private long id = 0;
	public String nextId() {
		return new StringBuilder(10).append('s').append(id++).toString();
	}

	protected void setId(RenderedComponent component, String widgetId) {
		component.getOutput().append(" id=\"").append((widgetId == null) ? nextId() : widgetId).append('\"');
	}
	
	@SuppressWarnings("static-method")
	protected String getClassName() {
		return "class";
	}
	
	protected void setDisabled(String disabledConditionName) {
		if (disabledConditionName != null) {
// TODO disabled
		}
	}

	protected void setInvisible(String invisibleConditionName, String extraELToAnd) {
		if (invisibleConditionName != null) {
//			String visible = BindUtil.negateCondition(invisibleConditionName);
// TODO invisible
		}
	}

	protected void setTextAlign(RenderedComponent component, HorizontalAlignment textAlignment) {
		String styleClass = null;
		if (HorizontalAlignment.left.equals(textAlignment)) {
			styleClass = "left";
		} 
		else if (HorizontalAlignment.centre.equals(textAlignment)) {
			styleClass = "center";
		} 
		else if (HorizontalAlignment.right.equals(textAlignment)) {
			styleClass = "right";
		}
		if (styleClass != null) {
			component.getOutput().append(' ').append(getClassName()).append("=\"").append(styleClass).append('\"');
		}
	}
	
	protected void setSize(RenderedComponent component,
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
		component.getOutput().append(" style=\"").append(style).append('\"');
	}
	
	protected void panelGroup(boolean nowrap, 
											boolean middle, 
											boolean blockLayout,
											String invisibleConditionName,
											String widgetId) {
// TOD panelGroup
/*
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
*/
	}
	
	protected void column(String invisible, 
								boolean noWrap, 
								boolean top, 
								Integer pixelWidth, 
								Integer responsiveWidth,
								Integer percentageWidth,
								Integer colspan, 
								Integer rowspan) {
// TODO column
/*
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
*/
	}
	
	protected void message(String forId) {
// TODO message
/*
		Message message = (Message) a.createComponent(Message.COMPONENT_TYPE);
		setId(message, null);
		message.setFor(forId);
		message.setShowDetail(true);
		message.setShowSummary(false);
		message.setDisplay("icon");
		message.setEscape(false);

		return message;
*/
	}
}
