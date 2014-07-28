package org.umlgraph.doclet;

import java.util.Map;
import java.util.regex.Pattern;

public class AccessibleOptions {
	private Options options;
	
	public AccessibleOptions(Options options) {
		this.options = options;
	}

	public Options getOptions() {
		return options;
	}

	public void hide() {
		options.setOption(new String[] { "-hide" });
	}
	
	public boolean isShowQualified() {
		return options.showQualified;
	}
	public void setShowQualified(boolean showQualified) {
		options.showQualified = showQualified;
	}
	public boolean isShowAttributes() {
		return options.showAttributes;
	}
	public void setShowAttributes(boolean showAttributes) {
		options.showAttributes = showAttributes;
	}
	public boolean isShowEnumerations() {
		return options.showEnumerations;
	}
	public void setShowEnumerations(boolean showEnumerations) {
		options.showEnumerations = showEnumerations;
	}
	public boolean isShowEnumConstants() {
		return options.showEnumConstants;
	}
	public void setShowEnumConstants(boolean showEnumConstants) {
		options.showEnumConstants = showEnumConstants;
	}
	public boolean isShowOperations() {
		return options.showOperations;
	}
	public void setShowOperations(boolean showOperations) {
		options.showOperations = showOperations;
	}
	public boolean isShowConstructors() {
		return options.showConstructors;
	}
	public void setShowConstructors(boolean showConstructors) {
		options.showConstructors = showConstructors;
	}
	public boolean isShowVisibility() {
		return options.showVisibility;
	}
	public void setShowVisibility(boolean showVisibility) {
		options.showVisibility = showVisibility;
	}
	public boolean isHorizontal() {
		return options.horizontal;
	}
	public void setHorizontal(boolean horizontal) {
		options.horizontal = horizontal;
	}
	public boolean isShowType() {
		return options.showType;
	}
	public void setShowType(boolean showType) {
		options.showType = showType;
	}
	public boolean isShowComment() {
		return options.showComment;
	}
	public void setShowComment(boolean showComment) {
		options.showComment = showComment;
	}
	public String getEdgeFontName() {
		return options.edgeFontName;
	}
	public void setEdgeFontName(String edgeFontName) {
		options.edgeFontName = edgeFontName;
	}
	public String getEdgeFontColor() {
		return options.edgeFontColor;
	}
	public void setEdgeFontColor(String edgeFontColor) {
		options.edgeFontColor = edgeFontColor;
	}
	public String getEdgeColor() {
		return options.edgeColor;
	}
	public void setEdgeColor(String edgeColor) {
		options.edgeColor = edgeColor;
	}
	public double getEdgeFontSize() {
		return options.edgeFontSize;
	}
	public void setEdgeFontSize(double edgeFontSize) {
		options.edgeFontSize = edgeFontSize;
	}
	public String getNodeFontName() {
		return options.nodeFontName;
	}
	public void setNodeFontName(String nodeFontName) {
		options.nodeFontName = nodeFontName;
	}
	public String getNodeFontAbstractName() {
		return options.nodeFontAbstractName;
	}
	public void setNodeFontAbstractName(String nodeFontAbstractName) {
		options.nodeFontAbstractName = nodeFontAbstractName;
	}
	public String getNodeFontColor() {
		return options.nodeFontColor;
	}
	public void setNodeFontColor(String nodeFontColor) {
		options.nodeFontColor = nodeFontColor;
	}
	public double getNodeFontSize() {
		return options.nodeFontSize;
	}
	public void setNodeFontSize(double nodeFontSize) {
		options.nodeFontSize = nodeFontSize;
	}
	public String getNodeFillColor() {
		return options.nodeFillColor;
	}
	public void setNodeFillColor(String nodeFillColor) {
		options.nodeFillColor = nodeFillColor;
	}
	public double getNodeFontClassSize() {
		return options.nodeFontClassSize;
	}
	public void setNodeFontClassSize(double nodeFontClassSize) {
		options.nodeFontClassSize = nodeFontClassSize;
	}
	public String getNodeFontClassName() {
		return options.nodeFontClassName;
	}
	public void setNodeFontClassName(String nodeFontClassName) {
		options.nodeFontClassName = nodeFontClassName;
	}
	public String getNodeFontClassAbstractName() {
		return options.nodeFontClassAbstractName;
	}
	public void setNodeFontClassAbstractName(String nodeFontClassAbstractName) {
		options.nodeFontClassAbstractName = nodeFontClassAbstractName;
	}
	public double getNodeFontTagSize() {
		return options.nodeFontTagSize;
	}
	public void setNodeFontTagSize(double nodeFontTagSize) {
		options.nodeFontTagSize = nodeFontTagSize;
	}
	public String getNodeFontTagName() {
		return options.nodeFontTagName;
	}
	public void setNodeFontTagName(String nodeFontTagName) {
		options.nodeFontTagName = nodeFontTagName;
	}
	public double getNodeFontPackageSize() {
		return options.nodeFontPackageSize;
	}
	public void setNodeFontPackageSize(double nodeFontPackageSize) {
		options.nodeFontPackageSize = nodeFontPackageSize;
	}
	public String getNodeFontPackageName() {
		return options.nodeFontPackageName;
	}
	public void setNodeFontPackageName(String nodeFontPackageName) {
		options.nodeFontPackageName = nodeFontPackageName;
	}
	public Shape getShape() {
		return options.shape;
	}
	public void setShape(Shape shape) {
		options.shape = shape;
	}
	public String getBgColor() {
		return options.bgColor;
	}
	public void setBgColor(String bgColor) {
		options.bgColor = bgColor;
	}
	public String getOutputFileName() {
		return options.outputFileName;
	}
	public void setOutputFileName(String outputFileName) {
		options.outputFileName = outputFileName;
	}
	public String getOutputEncoding() {
		return options.outputEncoding;
	}
	public void setOutputEncoding(String outputEncoding) {
		options.outputEncoding = outputEncoding;
	}
	public Map<Pattern, String> getApiDocMap() {
		return options.apiDocMap;
	}
	public void setApiDocMap(Map<Pattern, String> apiDocMap) {
		options.apiDocMap = apiDocMap;
	}
	public String getApiDocRoot() {
		return options.apiDocRoot;
	}
	public void setApiDocRoot(String apiDocRoot) {
		options.apiDocRoot = apiDocRoot;
	}
	public boolean isPostfixPackage() {
		return options.postfixPackage;
	}
	public void setPostfixPackage(boolean postfixPackage) {
		options.postfixPackage = postfixPackage;
	}
	public boolean isUseGuillemot() {
		return options.useGuillemot;
	}
	public void setUseGuillemot(boolean useGuillemot) {
		options.useGuillemot = useGuillemot;
	}
	public boolean isFindViews() {
		return options.findViews;
	}
	public void setFindViews(boolean findViews) {
		options.findViews = findViews;
	}
	public String getViewName() {
		return options.viewName;
	}
	public void setViewName(String viewName) {
		options.viewName = viewName;
	}
	public double getNodeSep() {
		return options.nodeSep;
	}
	public void setNodeSep(double nodeSep) {
		options.nodeSep = nodeSep;
	}
	public double getRankSep() {
		return options.rankSep;
	}
	public void setRankSep(double rankSep) {
		options.rankSep = rankSep;
	}
	public String getOutputDirectory() {
		return options.outputDirectory;
	}
	public void setOutputDirectory(String outputDirectory) {
		options.outputDirectory = outputDirectory;
	}
	public String getGuilOpen() {
		return options.guilOpen;
	}
	public void setGuilOpen(String guilOpen) {
		options.guilOpen = guilOpen;
	}
	public String getGuilClose() {
		return options.guilClose;
	}
	public void setGuilClose(String guilClose) {
		options.guilClose = guilClose;
	}
	public boolean isInferRelationships() {
		return options.inferRelationships;
	}
	public void setInferRelationships(boolean inferRelationships) {
		options.inferRelationships = inferRelationships;
	}
	public boolean isInferDependencies() {
		return options.inferDependencies;
	}
	public void setInferDependencies(boolean inferDependencies) {
		options.inferDependencies = inferDependencies;
	}
	public boolean isCollapsibleDiagrams() {
		return options.collapsibleDiagrams;
	}
	public void setCollapsibleDiagrams(boolean collapsibleDiagrams) {
		options.collapsibleDiagrams = collapsibleDiagrams;
	}
	public RelationPattern getContextRelationPattern() {
		return options.contextRelationPattern;
	}
	public void setContextRelationPattern(RelationPattern contextRelationPattern) {
		options.contextRelationPattern = contextRelationPattern;
	}
	public boolean isUseImports() {
		return options.useImports;
	}
	public void setUseImports(boolean useImports) {
		options.useImports = useImports;
	}
	public Visibility getInferDependencyVisibility() {
		return options.inferDependencyVisibility;
	}
	public void setInferDependencyVisibility(Visibility inferDependencyVisibility) {
		options.inferDependencyVisibility = inferDependencyVisibility;
	}
	public boolean isInferDepInPackage() {
		return options.inferDepInPackage;
	}
	public void setInferDepInPackage(boolean inferDepInPackage) {
		options.inferDepInPackage = inferDepInPackage;
	}
	public RelationType getInferRelationshipType() {
		return options.inferRelationshipType;
	}
	public void setInferRelationshipType(RelationType inferRelationshipType) {
		options.inferRelationshipType = inferRelationshipType;
	}
	public boolean isCompact() {
		return options.compact;
	}
	public void setCompact(boolean compact) {
		options.compact = compact;
	}
	public boolean isRelativeLinksForSourcePackages() {
		return options.relativeLinksForSourcePackages;
	}
	public void setRelativeLinksForSourcePackages(boolean relativeLinksForSourcePackages) {
		options.relativeLinksForSourcePackages = relativeLinksForSourcePackages;
	}
	public boolean isStrictMatching() {
		return options.strictMatching;
	}
	public void setStrictMatching(boolean strictMatching) {
		options.strictMatching = strictMatching;
	}
	public String getDotExecutable() {
		return options.dotExecutable;
	}
	public void setDotExecutable(String dotExecutable) {
		options.dotExecutable = dotExecutable;
	}
}
