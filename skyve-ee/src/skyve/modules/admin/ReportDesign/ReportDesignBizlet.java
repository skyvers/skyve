package modules.admin.ReportDesign;

import java.util.ArrayList;
import java.util.List;

import org.skyve.CORE;
import org.skyve.impl.generate.jasperreports.DesignSpecification;
import org.skyve.impl.generate.jasperreports.ReportBand.SplitType;
import org.skyve.impl.generate.jasperreports.ReportElement.ElementAlignment;
import org.skyve.impl.generate.jasperreports.ReportField;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.persistence.Persistence;
import org.skyve.util.Util;
import org.skyve.web.WebContext;

import modules.admin.domain.ReportDesign;
import modules.admin.domain.ReportDesign.CollectionType;
import modules.admin.domain.ReportDesign.DefinitionSource;
import modules.admin.domain.ReportDesign.Mode;
import modules.admin.domain.ReportDesign.Orientation;
import modules.admin.domain.ReportDesign.ReportType;

public class ReportDesignBizlet extends Bizlet<ReportDesign> {
	private static final long serialVersionUID = -6842667143963576852L;

	@Override
	public ReportDesign newInstance(ReportDesign bean) throws Exception {
		return super.newInstance(beanDesignFromSpecification(bean, new DesignSpecification()));
	}

	/**
	 * copy fields from conceptual specification to this report design
	 * 
	 * @param spec
	 * @return
	 */
	public static ReportDesign beanDesignFromSpecification(ReportDesign result, DesignSpecification spec) throws Exception {

		result.setName(spec.getName());
		if (spec.getMode() != null) {
			result.setMode(Mode.valueOf(spec.getMode().name()));
		}
		if (spec.getDefinitionSource() != null) {
			result.setDefinitionSource(DefinitionSource.valueOf(spec.getDefinitionSource().name()));
		}
		if (spec.getReportType() != null) {
			result.setReportType(ReportType.valueOf(spec.getReportType().name()));
			Util.LOGGER.info("RESULT REPORT TYPE IS " + result.getReportType().toDescription());
		}
		result.setModuleName(spec.getModuleName());
		result.setDocumentName(spec.getDocumentName());
		result.setRepositoryPath(spec.getRepositoryPath());
		result.setSaveToDocumentPackage(spec.getSaveToDocumentPackage());
		if (spec.getOrientation() != null) {
			result.setOrientation(Orientation.valueOf(spec.getOrientation().name()));
		}
		result.setWidth(spec.getWidth());
		result.setHeight(spec.getHeight());
		result.setLeftMargin(spec.getLeftMargin());
		result.setRightMargin(spec.getRightMargin());
		result.setTopMargin(spec.getTopMargin());
		result.setBottomMargin(spec.getBottomMargin());
		result.setColumnWidth(spec.getColumnWidth());
		result.setDefaultFontName(spec.getDefaultFontName());
		result.setTitleFontSize(spec.getTitleFontSize());
		result.setDefaultFontSize(spec.getDefaultFontSize());
		result.setRenderLabelAsTextFields(spec.getRenderLabelAsTextFields());
		result.setDefaultLineColour(spec.getDefaultLineColour());
		result.setDefaultLineWidth(spec.getDefaultLineWidth());
		result.setDefaultBorder(spec.getDefaultBorder());
		result.setDefaultBorderTop(spec.getDefaultBorderTop());
		result.setDefaultBorderLeft(spec.getDefaultBorderLeft());
		result.setDefaultBorderBottom(spec.getDefaultBorderBottom());
		result.setDefaultBorderRight(spec.getDefaultBorderRight());
		result.setDefaultElementHeight(spec.getDefaultElementHeight());
		result.setIncludePageNumbers(spec.getIncludePageNumbers());
		result.setDefaultCellTopPadding(spec.getDefaultCellTopPadding());
		result.setDefaultCellLeftPadding(spec.getDefaultCellLeftPadding());
		result.setDefaultCellBottomPadding(spec.getDefaultCellBottomPadding());
		result.setDefaultCellRightPadding(spec.getDefaultCellRightPadding());
		result.setDynamicFlow(spec.getDynamicFlow());
		if(spec.getBandSplitType()!=null){
			result.setBandSplitType(spec.getBandSplitType().toString());
		}
		result.setBoldLabels(spec.getBoldLabels());
		result.setCheckBoxFontName(spec.getCheckBoxFontName());
		result.setCheckBoxDisplayExpression(spec.getCheckBoxDisplayExpression());
		result.setPixelToTwip(spec.getPixelToTwip());
		result.setSectionBorderTop(spec.getSectionBorderTop());
		result.setSectionBorderLeft(spec.getSectionBorderLeft());
		result.setSectionBorderRight(spec.getSectionBorderRight());
		result.setSectionBorderBottom(spec.getSectionBorderBottom());
		result.setSectionTitleBorderTop(spec.getSectionTitleBorderTop());
		result.setSectionTitleBorderLeft(spec.getSectionTitleBorderLeft());
		result.setSectionTitleBorderRight(spec.getSectionTitleBorderRight());
		result.setSectionTitleBorderBottom(spec.getSectionTitleBorderBottom());
		result.setSectionTitleForeground(spec.getSectionTitleForeground());
		result.setSectionTitleBackground(spec.getSectionTitleBackground());
		if (spec.getField() != null) {
			result.setField(spec.getField().getDisplayName());
		}
		if (spec.getCollectionType() != null) {
			result.setCollectionType(CollectionType.valueOf(spec.getCollectionType().name()));
		}
		result.setParentReportPersistentName(spec.getParentReportPersistentName());
		result.setVerticalise(spec.getVerticalise());
		if (spec.getLabelAlignmentOverride() != null) {
			result.setLabelAlignmentOverride(spec.getLabelAlignmentOverride().toString());
		}

		return result;
	}

	/**
	 * copy fields from conceptual specification to this report design
	 * 
	 * @param spec
	 * @return
	 */
	public static DesignSpecification specificationFromDesignBean(ReportDesign spec) throws Exception {

		DesignSpecification result = new DesignSpecification();
		
		result.setName(spec.getName());
		if (spec.getMode() != null) {
			result.setMode(org.skyve.impl.generate.jasperreports.DesignSpecification.Mode.valueOf(spec.getMode().name()));
		}
		if (spec.getDefinitionSource() != null) {
			result.setDefinitionSource(org.skyve.impl.generate.jasperreports.DesignSpecification.DefinitionSource.valueOf(spec.getDefinitionSource().name()));
		}
		if (spec.getReportType() != null) {
			result.setReportType(org.skyve.impl.generate.jasperreports.DesignSpecification.ReportType.valueOf(spec.getReportType().name()));
			Util.LOGGER.info("SPEC REPORT TYPE IS " + result.getReportType().toString());
		}
		result.setModuleName(spec.getModuleName());
		result.setDocumentName(spec.getDocumentName());
		result.setRepositoryPath(spec.getRepositoryPath());
		result.setSaveToDocumentPackage(spec.getSaveToDocumentPackage());
		if (spec.getOrientation() != null) {
			result.setOrientation(org.skyve.impl.generate.jasperreports.DesignSpecification.Orientation.valueOf(spec.getOrientation().name()));
		}
		result.setWidth(spec.getWidth());
		result.setHeight(spec.getHeight());
		result.setLeftMargin(spec.getLeftMargin());
		result.setRightMargin(spec.getRightMargin());
		result.setTopMargin(spec.getTopMargin());
		result.setBottomMargin(spec.getBottomMargin());
		result.setColumnWidth(spec.getColumnWidth());
		result.setDefaultFontName(spec.getDefaultFontName());
		result.setTitleFontSize(spec.getTitleFontSize());
		result.setDefaultFontSize(spec.getDefaultFontSize());
		result.setRenderLabelAsTextFields(spec.getRenderLabelAsTextFields());
		result.setDefaultLineColour(spec.getDefaultLineColour());
		result.setDefaultLineWidth(spec.getDefaultLineWidth());
		result.setDefaultBorder(spec.getDefaultBorder());
		result.setDefaultBorderTop(spec.getDefaultBorderTop());
		result.setDefaultBorderLeft(spec.getDefaultBorderLeft());
		result.setDefaultBorderBottom(spec.getDefaultBorderBottom());
		result.setDefaultBorderRight(spec.getDefaultBorderRight());
		result.setDefaultElementHeight(spec.getDefaultElementHeight());
		result.setIncludePageNumbers(spec.getIncludePageNumbers());
		result.setDefaultCellTopPadding(spec.getDefaultCellTopPadding());
		result.setDefaultCellLeftPadding(spec.getDefaultCellLeftPadding());
		result.setDefaultCellBottomPadding(spec.getDefaultCellBottomPadding());
		result.setDefaultCellRightPadding(spec.getDefaultCellRightPadding());
		result.setDynamicFlow(spec.getDynamicFlow());
		if(spec.getBandSplitType()!=null){
			result.setBandSplitType(SplitType.valueOf(spec.getBandSplitType().toLowerCase()));
		}
		result.setBoldLabels(spec.getBoldLabels());
		result.setCheckBoxFontName(spec.getCheckBoxFontName());
		result.setCheckBoxDisplayExpression(spec.getCheckBoxDisplayExpression());
		result.setPixelToTwip(spec.getPixelToTwip());
		result.setSectionBorderTop(spec.getSectionBorderTop());
		result.setSectionBorderLeft(spec.getSectionBorderLeft());
		result.setSectionBorderRight(spec.getSectionBorderRight());
		result.setSectionBorderBottom(spec.getSectionBorderBottom());
		result.setSectionTitleBorderTop(spec.getSectionTitleBorderTop());
		result.setSectionTitleBorderLeft(spec.getSectionTitleBorderLeft());
		result.setSectionTitleBorderRight(spec.getSectionTitleBorderRight());
		result.setSectionTitleBorderBottom(spec.getSectionTitleBorderBottom());
		result.setSectionTitleForeground(spec.getSectionTitleForeground());
		result.setSectionTitleBackground(spec.getSectionTitleBackground());
		if (spec.getField() != null) {
			for(ReportField f: result.getFields()){
				if(f.getName().equals(spec.getField())){
					result.setField(f);
					break;
				}
			}
			
		}
		if (spec.getCollectionType() != null) {
			result.setCollectionType(org.skyve.metadata.model.document.Collection.CollectionType.valueOf(spec.getCollectionType().name()));
		}
		result.setParentReportPersistentName(spec.getParentReportPersistentName());
		result.setVerticalise(spec.getVerticalise());
		if (spec.getLabelAlignmentOverride() != null) {
			result.setLabelAlignmentOverride(ElementAlignment.valueOf(spec.getLabelAlignmentOverride()));
		}

		return result;
	}

	
	@Override
	public List<DomainValue> getVariantDomainValues(String attributeName) throws Exception {

		List<DomainValue> result = new ArrayList<>();
		Persistence pers = CORE.getPersistence();

		Customer customer = pers.getUser().getCustomer();
		if (ReportDesign.moduleNamePropertyName.equals(attributeName)) {
			for (Module module : customer.getModules()) {
				result.add(new DomainValue(module.getName(), module.getTitle()));
			}
		}

		return result;
	}

	@Override
	public List<DomainValue> getDynamicDomainValues(String attributeName, ReportDesign bean) throws Exception {

		Persistence pers = CORE.getPersistence();
		List<DomainValue> result = new ArrayList<>();

		Customer customer = pers.getUser().getCustomer();

		if (ReportDesign.documentNamePropertyName.equals(attributeName) && bean.getModuleName() != null) {
			Module module = customer.getModule(bean.getModuleName());
			for (String documentName : module.getDocumentRefs().keySet()) {
				Document document = module.getDocument(customer, documentName);
				result.add(new DomainValue(document.getName(), document.getDescription()));
			}
		}

		return result;
	}

	@Override
	public void preRerender(String source, ReportDesign bean, WebContext webContext) throws Exception {

		if (ReportDesign.documentNamePropertyName.equals(source) && bean.getName() == null) {
			// set a default name
			bean.setName("rpt" + bean.getDocumentName());
		}
		if (ReportDesign.defaultBorderPropertyName.equals(source)) {
			bean.setDefaultBorderTop(bean.getDefaultBorder());
			bean.setDefaultBorderLeft(bean.getDefaultBorder());
			bean.setDefaultBorderBottom(bean.getDefaultBorder());
			bean.setDefaultBorderRight(bean.getDefaultBorder());
		}
		if (ReportDesign.dynamicFlowPropertyName.equals(source) && Boolean.TRUE.equals(bean.getDynamicFlow())) {
			bean.setRenderLabelAsTextFields(Boolean.TRUE);
		}

		bean = resetDesign(bean);

		super.preRerender(source, bean, webContext);
	}

	public static ReportDesign resetDesign(ReportDesign design) throws Exception {
		ReportDesign result = design;
		if (Orientation.portrait.equals(result.getOrientation())) {
			result.setWidth(595);
			result.setColumnWidth(595 - result.getLeftMargin() - result.getRightMargin());
			result.setHeight(842);
		} else {
			result.setWidth(842);
			result.setColumnWidth(842 - result.getLeftMargin() - result.getRightMargin());
			result.setHeight(595);
		}
		return result;
	}

	public static List<DomainValue> defaultBindingDomainValues(Document document, ReportDesign bean) throws Exception {

		List<DomainValue> result = new ArrayList<>();

		// add other fields
		for (Attribute a : document.getAttributes()) {
			switch (a.getAttributeType()) {
			case collection:
				result.add(new DomainValue(a.getName()));
				break;
			case association:
				result.add(new DomainValue(a.getName() + ".bizKey"));
				break;
			default:
				result.add(new DomainValue(a.getName()));
				break;
			}
		}

		return result;
	}

}
