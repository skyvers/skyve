package org.skyve.content;

import java.util.Map;
import java.util.TreeMap;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Encapsulates the main possible mime types, their content type and file suffixes
 */
public enum MimeType {
	access("application/x-msaccess", "mdb"),
	aiff("audio/x-aiff", "aiff", "aif", "aifc"), 
	asf("video/x-ms-asf", "asf", "asr", "asx"), 
	avi("video/x-msvideo", "avi"), 
	axs("application/olescript", "axs"), 
	bcpio("application/x-bcpio", "bcpio"), 
	binhex("application/mac-binhex40", "hqx"), 
	bitmap("image/bmp", "bmp"), 
	cardfile("application/x-mscardfile", "crd"), 
	cat("application/vnd.ms-pkiseccat", "cat"), 
	cdf("application/x-cdf", "cdf"), 
	clip("application/x-msclip", "clp"), 
	cmx("image/x-cmx", "cmx"), 
	cod("image/cis-cod", "cod"), 
	component("text/x-component", "htc"), 
	cpio("application/x-cpio", "cpio"), 
	crl("application/pkix-crl", "crl"), 
	csh("application/x-csh", "csh"), 
	css("text/css", "css"), 
	csv("text/csv", "csv"),
	cal("text/calendar", "cal"),
	director("application/x-director", "dcr", "dir", "dxr"), 
	dll("application/x-msdownload", "dll"), 
	docm("application/vnd.ms-word.document.macroEnabled.12", "docm"), 
	docx("application/vnd.openxmlformats-officedocument.wordprocessingml.document", "docx"), 
	dotm("application/vnd.ms-word.template.macroEnabled.12", "dotm"), 
	dotx("application/vnd.openxmlformats-officedocument.wordprocessingml.template", "dotx"), 
	dvi("application/x-dvi", "dvi"), 
	envoy("application/envoy", "evy"), 
	excel("application/vnd.ms-excel", "xls", "xlt", "xla", "xlc", "xlm", "xlw"), 
	flash("application/x-shockwave-flash", "swf"), 
	fractals("application/fractals", "fif"), 
	futureSplash("application/futuresplash", "spl"), 
	gif("image/gif", "gif"), 
	gtar("application/x-gtar", "gtar"), 
	gzip("application/gzip", "gz"), 
	hdf("application/x-hdf", "hdf"), 
	helpfile("application/winhlp", "hlp"), 
	h323("text/h323", "323"), 
	hta("application/hta", "hta"), 
	html("text/html", "html", "htm", "shtml", "stm"), 
	icon("image/x-icon", "ico"), 
	ief("image/ief", "ief"), 
	internetPropertyStream("application/internet-property-stream", "acx"), 
	internetSignup("application/x-internet-signup", "isp", "ins"), 
	iphone("application/x-iphone", "iii"), 
	iuls("text/iuls", "uls"), 
	javascript("application/javascript", "js"), 
	jpeg("image/jpeg", "jpeg", "jpe", "jpg"), 
	json("application/json", "json"),
	laasf("video/x-la-asf", "lsx", "lsf"), 
	latex("application/x-latex", "latex"), 
	man("application/x-troff-man", "man"), 
	me("application/x-troff-me", "me"), 
	mediaView("application/x-msmediaview", "mvb", "m13", "m14"), 
	metafile("application/x-msmetafile", "wmf"), 
	mht("message/rfc822", "mht", "mhtml", "nws"), 
	midi("audio/mid", "mid", "rmi"), 
	money("application/x-msmoney", "mny"), 
	movie("video/x-sgi-movie", "movie"), 
	mp3("audio/mpeg", "mp3"), 
	mpeg("video/mpeg", "mpeg", "mpg", "mp2", "mpa", "mpe", "mpv2"), 
	mpegUrl("audio/x-mpegurl", "m3u"), 
	ms("application/x-troff-ms", "ms"), 
	octetStream("application/octet-stream", "bin", "class", "dms", "exe", "lha", "lzh"), 
	oda("application/oda", "oda"),
	ogg("audio/ogg", "ogg"), 
	openDocumentText("application/vnd.oasis.opendocument.text", "odt", "odf"),
	openDocumentSpreadsheet("application/vnd.oasis.opendocument.spreadsheet", "ods"),
	pdf("application/pdf", "pdf"), 
	perfmon("application/x-perfmon", "pma", "pmc", "pml", "pmr", "pmw"), 
	picsRules("application/pics-rules", "prf"), 
	pipeg("image/pipeg", "jfif"), 
	pixmap("image/x-xpixmap", "xpm"), 
	pkcs10("application/pkcs10", "p10"), 
	pkcs12("application/x-pkcs12", "p12", "pfx"), 
	pkcs7Certificates("application/x-pkcs7-certificates", "p7b", "spc"), 
	pkcs7CertRegResp("application/x-pkcs7-certreqresp", "p7r"), 
	pkcs7Mime("application/x-pkcs7-mime", "p7m", "p7c"), 
	pkcs7Signature("application/x-pkcs7-signature", "p7s"), 
	pkiCertStore("application/vnd.ms-pkicertstore", "sst"), 
	pkipko("application/ynd.ms-pkipko", "pko"), 
	pkistl("application/vnd.ms-pkistl", "stl"), 
	plain("text/plain", "txt", "", "bas", "c", "h"), 
	portableAnymap("image/x-portable-anymap", "pnm"), 
	portableBitmap("image/x-portable-bitmap", "pbm"), 
	portableGraymap("image/x-portable-graymap", "pgm"), 
	png("image/png", "png"),
	portablePixmap("image/x-portable-pixmap", "ppm"), 
	postscript("application/postscript", "ps", "ai", "eps"), 
	potm("application/vnd.ms-powerpoint.template.macroEnabled.12", "potm"), 
	potx("application/vnd.openxmlformats-officedocument.presentationml.template", "potx"), 
	powerpoint("application/vnd.ms-powerpoint", "ppt", "pot", "pps", "ppa"), 
	ppam("application/vnd.ms-powerpoint.addin.macroEnabled.12", "ppam"), 
	ppsm("application/vnd.ms-powerpoint.slideshow.macroEnabled.12", "ppsm"), 
	ppsx("application/vnd.openxmlformats-officedocument.presentationml.slideshow", "ppsx"), 
	pptm("application/vnd.ms-powerpoint.presentation.macroEnabled.12", "pptm"), 
	pptx("application/vnd.openxmlformats-officedocument.presentationml.presentation", "pptx"), 
	project("application/vnd.ms-project", "mpp"), 
	publisher("application/x-mspublisher", "pub"), 
	quicktime("video/quicktime", "mov", "qt"), 
	raster("image/x-cmu-raster", "ras"), 
	realAudio("audio/x-pn-realaudio", "ra", "ram"), 
	rgb("image/x-rgb", "rgb"), 
	rtf("application/rtf", "rtf"), 
	richtext("text/richtext", "rtx"), 
	schedule("application/x-msschedule", "scd"), 
	scriptlet("text/scriptlet", "sct"), 
	setext("text/x-setext", "etx"), 
	setPay("application/set-payment-initiation", "setpay"), 
	setReg("application/set-registration-initiation", "setreg"), 
	sh("application/x-sh", "sh"), 
	shar("application/x-shar", "shar"), 
	snd("audio/basic", "snd", "au"), 
	source("application/x-wais-source", "src"), 
	stuffit("application/x-stuffit", "stuffit", "sit"), 
	sv4cpio("application/x-sv4cpio", "sv4cpio"), 
	sv4crc("application/x-sv4crc", "sv4crc"), 
	svg("image/svg+xml", "svg"), 
	tar("application/x-tar", "tar"), 
	tcl("application/x-tcl", "tcl"), 
	terminal("application/x-msterminal", "trm"), 
	tex("application/x-tex", "tex"), 
	texInfo("application/x-texinfo", "texinfo", "texi"), 
	tgz("application/x-compressed", "tar.gz", "tgz"), 
	tiff("image/tiff", "tiff", "tif"), 
	troff("application/x-troff", "roff", "t", "tr"), 
	tsv("text/tab-separated-values", "tsv"), 
	ustar("application/x-ustar", "ustar"), 
	vcard("text/x-vcard", "vcf"), 
	vrml("x-world/x-vrml", "vrml", "flr", "wrl", "wrz", "xaf", "xof"), 
	wav("audio/x-wav", "wav"), 
	webviewhtml("text/webviewhtml", "htt"), 
	windowDump("image/x-xwindowdump", "xwd"), 
	works("application/vnd.ms-works", "wks", "wcm", "wdb", "wps"), 
	word("application/msword", "doc", "dot"), 
	write("application/x-mswrite", "wri"), 
	x509Certificate("application/x-x509-ca-cert", "cer", "crt", "der"), 
	xbitmap("image/x-xbitmap", "xbm"), 
	xlam("application/vnd.ms-excel.addin.macroEnabled.12", "xlam"), 
	xlsb("application/vnd.ms-excel.sheet.binary.macroEnabled.12", "xlsb"), 
	xlsm("application/vnd.ms-excel.sheet.macroEnabled.12", "xlsm"), 
	xlsx("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet", "xlsx"), 
	xltm("application/vnd.ms-excel.template.macroEnabled.12", "xltm"), 
	xltx("application/vnd.openxmlformats-officedocument.spreadsheetml.template", "xltx"), 
	xml("application/xml", "xml"), 
	z("application/x-compress", "z"), 
	zip("application/zip", "zip");

	// Enum values are constructed before the statics
	private static Map<String, MimeType> fileExtensions = new TreeMap<>();
	private static Map<String, MimeType> contentTypes = new TreeMap<>();
	static {
		for (MimeType mimeType : values()) {
			contentTypes.put(mimeType.contentType, mimeType);
			fileExtensions.put(mimeType.standardFileSuffix, mimeType);
			for (String otherFileSuffix : mimeType.otherFileSuffixes) {
				fileExtensions.put(otherFileSuffix, mimeType);
			}
		}
	}
	
	// The content type of this mime type
	private String contentType;
	// The standard file suffix used for a file with this content
	private String standardFileSuffix;
	// Other file suffixes used for a file with this content
	private String[] otherFileSuffixes;

	/**
	 * Constructor
	 * @param contentType
	 * @param standardFileSuffix
	 * @param otherFileSuffixes
	 */
	private MimeType(@Nonnull String contentType, 
						@Nonnull String standardFileSuffix,
						@Nonnull String... otherFileSuffixes) {
		this.contentType = contentType;
		this.standardFileSuffix = standardFileSuffix;
		this.otherFileSuffixes = otherFileSuffixes;
	}

	/**
	 * Returns the content type.
	 */
	@Override
	public @Nonnull String toString() {
		return contentType;
	}

	/**
	 * Returns the standard file suffix
	 */
	public @Nonnull String getStandardFileSuffix() {
		return standardFileSuffix;
	}

	/**
	 * Get the mime type for the contentType
	 * @param contentType	The content type
	 * @return	The mime type or null if there is no mime type for the given content type
	 */
	public static @Nullable MimeType fromContentType(@Nonnull String contentType) {
		return contentTypes.get(contentType);
	}

	/**
	 * Get the mime type for the fileSuffix
	 * @param fileSuffix	The file suffix
	 * @return	The mime type or null if there is no mime type for the given file suffix.
	 */
	public static @Nullable MimeType fromFileSuffix(@Nonnull String fileSuffix) {
		return fileExtensions.get(fileSuffix);
	}

	/**
	 * Returns the Mime Type of the file, depending on the extension of the filename
	 * @return 	The mime type of the file or null if no mime type exists.
	 */
	public static @Nullable MimeType fromFileName(@Nonnull String fileName) {
		MimeType result = null;
		
		int dotIndex = fileName.lastIndexOf('.');
		if (dotIndex >= 0) {
			result = fromFileSuffix(fileName.substring(dotIndex + 1).toLowerCase());
		}
		
		return result;
	}
}
