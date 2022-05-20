(function() { var a, b = window.T, c = "http"; scheme = c + "://", host =
	"open.t.qq.com", a = { name: "Tencent weibo SDK", version: "1.0",
		appkey: { value: "{APPKEY}", version: "{APPKEY_VERSION}",
			verified: !1 }, debug: !0, pingback: !0, _domain: {
				api: scheme + host + "{API_URI}", auth: scheme
	+ host + "{AUTH_URI}", query: scheme + host + "{QUERY_TOKEN_URI}",
exchange: scheme + host + "{EXCHANGE_TOKEN_URI}", flashproxy: scheme + host +
	"{FLASHPROXY_URI}", serverproxy: scheme + host + "{SERVERPROXY_URI}",
clientproxy: "{CLIENTPROXY_URI}" }, _const: { AUTH_WINDOW_NAME:
	"authClientProxy_ee5a0f93", AUTH_WINDOW_WIDTH: 575, AUTH_WINDOW_HEIGHT:
	465 }, _cookie: { names: { accessToken: "QQWBToken", refreshToken:
		"QQWBRefreshToken" }, path: "/", domain: "" }, noConflict:
	function() { return b && (window.T = b), a }, copy: function(a, b, c,
		d) { for (var e in b) if (c || typeof a[e] == "undefined") a[e]
			= d ? d(b[e]) : b[e]; return a }, create: function(a,
				b) { var c = this, d = a ? a.split(".") : [], e
					= d.length; for (var f = 0; f < e; f++)
{ var g = d[f], h = c[g]; h || (h = b && f + 1 === e ? b : {}, c[g] = h), c = h
} return c }, extend: function(b, c, d) { return a.copy(typeof b == "string" ?
	a.create.call(this, b) : b, c, d) }, _alias: function(b, c) { c = c ||
		a; if (typeof b == "string") this[b] = c; else if
			(Object.prototype.toString.call(b) === "[object
			 Array]") for (var d = 0, e = b.length; d < e; d++)
			 this[b[d]] = c }, alias: function(b, c) { a._alias(b,
					 a[c]) }, assign: function(a, b, c) {
						 var d = this, e = d, f = a ?
							 a.split(".") : [], g =
							 f.length; for (var h =
									 0; h <
									 g;
									 h++) {
										 var
											 i
											 =
											 f[h],
											 j
												 =
												 d[i];
										 if
											 (!j)
												 throw
												 new
												 Error("Tencent
														 weibo
														 SDK:
														 [ERROR]
														 no
														 such
														 name
														 "
														 +
														 i);
										 e
											 =
											 d,
											 d
												 =
												 j
									 } if
						 (typeof d == "string") e[i] =
							 d.replace(new
									 RegExp("\\{"
										 +
										 b
										 +
										 "\\}",
										 "ig"),
									 c);
						 else if (typeof d == "object")
							 for (var k in d)
								 d.hasOwnProperty(k)
									 &&
									 typeof
									 d[k]
									 ==
									 "string"
									 &&
									 (d[k]
									  =
									  d[k].replace(new
										  RegExp("\\{"
											  +
											  b
											  +
											  "\\}",
											  "ig"),
										  c))
					 }, uid: function() { return
						 Math.random().toString(16).substr(2)
					 } }, a.alias("provide", "create"),
					 a._alias.call(window, ["QQWB", "T"],
							 a),
					 a.assign("_domain", "API_URI",
							 "/api"),
					 a.assign("_domain", "AUTH_URI",
							 "/oauth2_html/login.php"),
					 a.assign("_domain", "SERVERPROXY_URI",
							 "/open-js/proxy.html"),
					 a.assign("_domain", "FLASHPROXY_URI",
							 "/open-js/proxy_v15.swf"),
					 a.assign("_domain",
							 "EXCHANGE_TOKEN_URI",
							 "/cgi-bin/exchange_token"),
					 a.assign("_domain", "QUERY_TOKEN_URI",
							 "/cgi-bin/auto_token")
					 })(), QQWB.extend("String", {
						 _trimLeft: /^\s+/, _trimRight:
						 /\s+$/, isString: function(a)
					 { return typeof a == "string" },
					 ltrim: function(a) { return a == null
						 ? "" :
						 a.toString().replace(this._trimLeft,
							 "") }, rtrim:
						 function(a) { return a == null
							 ? "" :
						 a.toString().replace(this._trimRight,
							 "") }, trim:
						 String.prototype.trim ?
						 function(a) { return a == null
							 ? "" :
						 String.prototype.trim.call(a)
						 } : function(a) { return a ==
							 null ? "" :
						 a.toString().replace(this._trimLeft,
							 "").replace(this._trimRight,
								 "") },
							 startsWith:
								 String.prototype.startsWith
								 ?  function(a,
										 b)
								 { return a ==
									 null ?
										 !1
										 :
										 String.prototype.startsWith.call(a,
												 b)
								 } :
					 function(a, b) { return a == null ? !1
						 : a.toString().indexOf(b) == 0
					 }, endsWith: String.prototype.endsWith
					 ?  function(a, b) { return a == null ?
						 !1 :
							 String.prototype.endsWith.call(a,
									 b) } :
							 function(a, b) {
								 return a ==
									 null ?
									 !1 :
									 a.toString().lastIndexOf(b)
									 >= 0
									 &&
									 a.toString().lastIndexOf(b)
									 +
									 b.length
									 ==
									 a.length
							 } }),
							 QQWB.extend("_const",
									 {
										 HTTP_METHOD_GET:
								 "GET",
							 HTTP_METHOD_POST:
								 "POST",
							 HTTP_METHOD_GET_OR_POST:
								 "GET | POST",
							 API_CATEGORY_TIMELINE:
								 "时间线",
							 API_CATEGORY_WEIBO:
								 "微博相关",
							 API_CATEGORY_ACCOUNT:
								 "账户相关",
							 API_CATEGORY_RELATION:
								 "关系链相关",
							 API_CATEGORY_SIXIN:
								 "私信相关",
							 API_CATEGORY_SEARCH:
								 "搜索相关",
							 API_CATEGORY_TRENS:
								 "热度趋势",
							 API_CATEGORY_QUERY:
								 "查看数据",
							 API_CATEGORY_FAVORITE:
								 "数据收藏",
							 API_CATEGORY_TOPIC:
								 "话题相关",
							 API_CATEGORY_TAG:
								 "标签相关",
							 API_CATEGORY_OTHER:
								 "其他",
							 API_NO_DESCRIPTION:
								 "暂时没有关于此参数的说明",
							 API_NO_DEFAULT_VALUE:
								 "",
							 COMMON_NULL: null,
							 COMMON_EMPTY_STRING:
								 "" }),
								 QQWB.extend("_apiProvider",
										 {
											 _apiRetError:
								 { 1:
									 "参数错误",
								 2: "频率受限",
								 3: "鉴权失败",
								 4: "内部错误"
								 },
								 _apiErrorCode:
								 { 4:
									 "过多脏话",
								 5: "禁止访问",
								 6:
									 "记录不存在",
								 8: "内容过长",
								 9:
									 "内容包含垃圾信息",
								 10:
									 "发表太快，频率限制",
								 11:
									 "源消息不存在",
								 12:
									 "未知错误",
								 13: "重复发表"
								 },
								 _apiParseRetCode:
									 function(a)
									 { var
										 b
											 =
											 a.match(/\"ret\":(\d+)\}/)
											 ||
											 a.match(/<ret>(\d+)<\/ret>/);
										 return
											 b
											 ?
											 parseInt(b[1],
													 10)
											 :
											 b
									 },
								 _apiParseErrorCode:
									 function(a)
									 { var
										 b
											 =
											 a.match(/\"errcode\":(-?\d+)/)
											 ||
											 a.match(/<errcode>(\d+)<\/errcode>/);
										 return
											 b
											 ?
											 parseInt(b[1],
													 10)
											 :
											 b
									 },
								 _apiGetErrorMessage:
									 function(a,
											 b)
									 { var
										 c
											 =
											 [],
										 a
											 =
											 a
											 +
											 "",
										 b
											 =
											 b
											 +
											 "",
										 d
											 =
											 QQWB._apiProvider._apiRetError[a],
										 e
											 =
											 QQWB._apiProvider._apiErrorCode[b];
										 return
											 d
											 &&
											 c.push(d),
											 e
												 &&
												 c.push(e),
											 c.length
												 >
												 0
												 ?
												 c.join(",")
												 :
												 "未知错误"
									 },
								 apis: {
									 "/statuses/home_timeline":
									 {
										 category:
											 QQWB._const.API_CATEGORY_TIMELINE,
										 description:
											 "主页时间线",
										 supportMethod:
											 QQWB._const.HTTP_METHOD_GET,
										 supportParams:
										 {
											 pageflag:
											 {
												 defaultValue:
													 "0",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 reqnum:
											 {
												 defaultValue:
													 20,
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 pagetime:
											 {
												 defaultValue:
													 "0",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 }
										 }
									 },
									 "/statuses/home_timeline_vip":
									 {
										 category:
											 QQWB._const.API_CATEGORY_TIMELINE,
										 description:
											 "vip用户时间线",
										 supportMethod:
											 QQWB._const.HTTP_METHOD_GET,
										 supportParams:
										 {
											 pageflag:
											 {
												 defaultValue:
													 "0",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 reqnum:
											 {
												 defaultValue:
													 "2",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 pagetime:
											 {
												 defaultValue:
													 "0",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 lastid:
											 {
												 defaultValue:
													 "",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 }
										 }
									 },
									 "/statuses/public_timeline":
									 {
										 category:
											 QQWB._const.API_CATEGORY_TIMELINE,
										 description:
											 "广播大厅时间线",
										 supportMethod:
											 QQWB._const.HTTP_METHOD_GET,
										 supportParams:
										 {
											 pos:
											 {
												 defaultValue:
													 "0",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 reqnum:
											 {
												 defaultValue:
													 20,
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 }
										 }
									 },
									 "/statuses/user_timeline":
									 {
										 category:
											 QQWB._const.API_CATEGORY_TIMELINE,
										 description:
											 "其他用户发表时间线",
										 supportMethod:
											 QQWB._const.HTTP_METHOD_GET,
										 supportParams:
										 {
											 pageflag:
											 {
												 defaultValue:
													 "0",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 pagetime:
											 {
												 defaultValue:
													 "0",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 reqnum:
											 {
												 defaultValue:
													 20,
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 lastid:
											 {
												 defaultValue:
													 "0",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 name:
											 {
												 defaultValue:
													 "t",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 }
										 }
									 },
									 "/statuses/mentions_timeline":
									 {
										 category:
											 QQWB._const.API_CATEGORY_TIMELINE,
										 description:
											 "@提到我的时间线",
										 supportMethod:
											 QQWB._const.HTTP_METHOD_GET,
										 supportParams:
										 {
											 pageflag:
											 {
												 defaultValue:
													 "0",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 pagetime:
											 {
												 defaultValue:
													 "0",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 reqnum:
											 {
												 defaultValue:
													 20,
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 lastid:
											 {
												 defaultValue:
													 "0",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 type:
											 {
												 defaultValue:
													 "0x1",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 }
										 }
									 },
									 "/statuses/ht_timeline":
									 {
										 category:
											 QQWB._const.API_CATEGORY_TIMELINE,
										 description:
											 "话题时间线",
										 supportMethod:
											 QQWB._const.HTTP_METHOD_POST,
										 supportParams:
										 {
											 httext:
											 {
												 defaultValue:
													 "pBoard",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 pageflag:
											 {
												 defaultValue:
													 "0",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 pageinfo:
											 {
												 defaultValue:
													 "",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 reqnum:
											 {
												 defaultValue:
													 20,
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 }
										 }
									 },
									 "/statuses/broadcast_timeline":
									 {
										 category:
											 QQWB._const.API_CATEGORY_TIMELINE,
										 description:
											 "我发表时间线",
										 supportMethod:
											 QQWB._const.HTTP_METHOD_GET,
										 supportParams:
										 {
											 pageflag:
											 {
												 defaultValue:
													 "0",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 pagetime:
											 {
												 defaultValue:
													 "0",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 reqnum:
											 {
												 defaultValue:
													 20,
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 lastid:
											 {
												 defaultValue:
													 "0",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 }
										 }
									 },
									 "/statuses/special_timeline":
									 {
										 category:
											 QQWB._const.API_CATEGORY_TIMELINE,
										 description:
											 "特别收听的人发表时间线",
										 supportMethod:
											 QQWB._const.HTTP_METHOD_GET,
										 supportParams:
										 {
											 pageflag:
											 {
												 defaultValue:
													 "0",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 pagetime:
											 {
												 defaultValue:
													 "0",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 reqnum:
											 {
												 defaultValue:
													 20,
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 }
										 }
									 },
									 "/statuses/broadcast_timeline_ids":
									 {
										 category:
											 QQWB._const.API_CATEGORY_TIMELINE,
										 description:
											 "我发表时间线索引",
										 supportMethod:
											 QQWB._const.HTTP_METHOD_GET,
										 supportParams:
										 {
											 pageflag:
											 {
												 defaultValue:
													 "0",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 pagetime:
											 {
												 defaultValue:
													 20,
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 reqnum:
											 {
												 defaultValue:
													 "",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 lastid:
											 {
												 defaultValue:
													 "",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 type:
											 {
												 defaultValue:
													 "",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 contenttype:
											 {
												 defaultValue:
													 "",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 accesslevel:
											 {
												 defaultValue:
													 "",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 }
										 }
									 },
									 "/statuses/home_timeline_ids":
									 {
										 category:
											 QQWB._const.API_CATEGORY_TIMELINE,
										 description:
											 "首页时间线索引",
										 supportMethod:
											 QQWB._const.HTTP_METHOD_GET,
										 supportParams:
										 {
											 pageflag:
											 {
												 defaultValue:
													 "0",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 pagetime:
											 {
												 defaultValue:
													 20,
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 reqnum:
											 {
												 defaultValue:
													 "",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 lastid:
											 {
												 defaultValue:
													 "",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 type:
											 {
												 defaultValue:
													 "",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 contenttype:
											 {
												 defaultValue:
													 "",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 accesslevel:
											 {
												 defaultValue:
													 "",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 }
										 }
									 },
									 "/statuses/mentions_timeline_ids":
									 {
										 category:
											 QQWB._const.API_CATEGORY_TIMELINE,
										 description:
											 "提及我的时间线索引",
										 supportMethod:
											 QQWB._const.HTTP_METHOD_GET,
										 supportParams:
										 {
											 pageflag:
											 {
												 defaultValue:
													 "0",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 pagetime:
											 {
												 defaultValue:
													 20,
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 reqnum:
											 {
												 defaultValue:
													 "",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 lastid:
											 {
												 defaultValue:
													 "",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 type:
											 {
												 defaultValue:
													 "",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 contenttype:
											 {
												 defaultValue:
													 "",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 accesslevel:
											 {
												 defaultValue:
													 "",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 }
										 }
									 },
									 "/statuses/user_timeline_ids":
									 {
										 category:
											 QQWB._const.API_CATEGORY_TIMELINE,
										 description:
											 "用户发表时间线索引",
										 supportMethod:
											 QQWB._const.HTTP_METHOD_GET,
										 supportParams:
										 {
											 pageflag:
											 {
												 defaultValue:
													 "0",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 pagetime:
											 {
												 defaultValue:
													 20,
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 reqnum:
											 {
												 defaultValue:
													 "",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 lastid:
											 {
												 defaultValue:
													 "",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 name:
											 {
												 defaultValue:
													 "t",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 type:
											 {
												 defaultValue:
													 "",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 contenttype:
											 {
												 defaultValue:
													 "",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 accesslevel:
											 {
												 defaultValue:
													 "",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 }
										 }
									 },
									 "/statuses/users_timeline":
									 {
										 category:
											 QQWB._const.API_CATEGORY_TIMELINE,
										 description:
											 "多用户发表时间线",
										 supportMethod:
											 QQWB._const.HTTP_METHOD_GET,
										 supportParams:
										 {
											 pageflag:
											 {
												 defaultValue:
													 "0",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 pagetime:
											 {
												 defaultValue:
													 20,
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 reqnum:
											 {
												 defaultValue:
													 "",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 lastid:
											 {
												 defaultValue:
													 "",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 names:
											 {
												 defaultValue:
													 "t,api_weibo",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 type:
											 {
												 defaultValue:
													 "",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 contenttype:
											 {
												 defaultValue:
													 "",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 accesslevel:
											 {
												 defaultValue:
													 "",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 }
										 }
									 },
									 "/statuses/users_timeline_ids":
									 {
										 category:
											 QQWB._const.API_CATEGORY_TIMELINE,
										 description:
											 "多用户发表时间线索引",
										 supportMethod:
											 QQWB._const.HTTP_METHOD_GET,
										 supportParams:
										 {
											 pageflag:
											 {
												 defaultValue:
													 "0",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 pagetime:
											 {
												 defaultValue:
													 20,
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 reqnum:
											 {
												 defaultValue:
													 "",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 lastid:
											 {
												 defaultValue:
													 "",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 names:
											 {
												 defaultValue:
													 "t,api_weibo",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 type:
											 {
												 defaultValue:
													 "",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 contenttype:
											 {
												 defaultValue:
													 "",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 accesslevel:
											 {
												 defaultValue:
													 "",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 }
										 }
									 },
									 "/statuses/area_timeline":
									 {
										 category:
											 QQWB._const.API_CATEGORY_TIMELINE,
										 description:
											 "同城发表时间线",
										 supportMethod:
											 QQWB._const.HTTP_METHOD_GET,
										 supportParams:
										 {
											 pos:
											 {
												 defaultValue:
													 "0",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 reqnum:
											 {
												 defaultValue:
													 20,
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 country:
											 {
												 defaultValue:
													 "1",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 province:
											 {
												 defaultValue:
													 11,
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 city:
											 {
												 defaultValue:
													 "1",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 }
										 }
									 },
									 "/statuses/ht_timeline_ext":
									 {
										 category:
											 QQWB._const.API_CATEGORY_TIMELINE,
										 description:
											 "话题时间线(修复翻页问题)",
										 supportMethod:
											 QQWB._const.HTTP_METHOD_GET,
										 supportParams:
										 {
											 reqnum:
											 {
												 defaultValue:
													 10,
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 pageflag:
											 {
												 defaultValue:
													 "0",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 flag:
											 {
												 defaultValue:
													 "0",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 accesslevel:
											 {
												 defaultValue:
													 "0",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 type:
											 {
												 defaultValue:
													 "0",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 contenttype:
											 {
												 defaultValue:
													 "0",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 httext:
											 {
												 defaultValue:
													 "iweibo",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 htid:
											 {
												 defaultValue:
													 "0",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 }
										 }
									 },
									 "/t/show":
									 {
										 category:
											 QQWB._const.API_CATEGORY_WEIBO,
										 description:
											 "获取一条微博数据",
										 supportMethod:
											 QQWB._const.HTTP_METHOD_GET,
										 supportParams:
										 {
											 id:
											 {
												 defaultValue:
													 51545056800467,
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 }
										 }
									 },
									 "/t/add":
									 {
										 category:
											 QQWB._const.API_CATEGORY_WEIBO,
										 description:
											 "发表一条微博",
										 supportMethod:
											 QQWB._const.HTTP_METHOD_POST,
										 supportParams:
										 {
											 content:
											 {
												 defaultValue:
													 "test",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 clientip:
											 {
												 defaultValue:
													 "127.0.0.1",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 jing:
											 {
												 defaultValue:
													 "",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 wei:
											 {
												 defaultValue:
													 "",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 }
										 }
									 },
									 "/t/del":
									 {
										 category:
											 QQWB._const.API_CATEGORY_WEIBO,
										 description:
											 "删除一条微博",
										 supportMethod:
											 QQWB._const.HTTP_METHOD_POST,
										 supportParams:
										 {
											 id:
											 {
												 defaultValue:
													 94035056272295,
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 }
										 }
									 },
									 "/t/re_add":
									 {
										 category:
											 QQWB._const.API_CATEGORY_WEIBO,
										 description:
											 "转播一条微博",
										 supportMethod:
											 QQWB._const.HTTP_METHOD_POST,
										 supportParams:
										 {
											 content:
											 {
												 defaultValue:
													 "test",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 clientip:
											 {
												 defaultValue:
													 "127.0.0.1",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 jing:
											 {
												 defaultValue:
													 "",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 wei:
											 {
												 defaultValue:
													 "",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 reid:
											 {
												 defaultValue:
													 77048060858014,
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 }
										 }
									 },
									 "/t/reply":
									 {
										 category:
											 QQWB._const.API_CATEGORY_WEIBO,
										 description:
											 "回复一条微博",
										 supportMethod:
											 QQWB._const.HTTP_METHOD_POST,
										 supportParams:
										 {
											 content:
											 {
												 defaultValue:
													 "test",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 clientip:
											 {
												 defaultValue:
													 "127.0.0.1",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 jing:
											 {
												 defaultValue:
													 "",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 wei:
											 {
												 defaultValue:
													 "",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 reid:
											 {
												 defaultValue:
													 77048060858014,
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 }
										 }
									 },
									 "/t/add_pic":
									 {
										 category:
											 QQWB._const.API_CATEGORY_WEIBO,
										 description:
											 "发表一条图片微博",
										 supportMethod:
											 QQWB._const.HTTP_METHOD_POST,
										 supportParams:
										 {
											 content:
											 {
												 defaultValue:
													 "test",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 clientip:
											 {
												 defaultValue:
													 "127.0.0.1",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 jing:
											 {
												 defaultValue:
													 "",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 wei:
											 {
												 defaultValue:
													 "",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 pic:
											 {
												 defaultValue:
													 "",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 }
										 }
									 },
									 "/t/add_emotion":
									 {
										 category:
											 QQWB._const.API_CATEGORY_WEIBO,
										 description:
											 "发表一条心情微博",
										 supportMethod:
											 QQWB._const.HTTP_METHOD_POST,
										 supportParams:
										 {
											 content:
											 {
												 defaultValue:
													 "test",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 clientip:
											 {
												 defaultValue:
													 "127.0.0.1",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 jing:
											 {
												 defaultValue:
													 "",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 wei:
											 {
												 defaultValue:
													 "",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 signtype:
											 {
												 defaultValue:
													 "0",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 }
										 }
									 },
									 "/t/re_count":
									 {
										 category:
											 QQWB._const.API_CATEGORY_WEIBO,
										 description:
											 "转播数或点评数",
										 supportMethod:
											 QQWB._const.HTTP_METHOD_POST,
										 supportParams:
										 {
											 ids:
											 {
												 defaultValue:
													 0xb04fd23c98500000,
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 flag:
											 {
												 defaultValue:
													 "0",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 }
										 }
									 },
									 "/t/re_list":
									 {
										 category:
											 QQWB._const.API_CATEGORY_WEIBO,
										 description:
											 "获取单条微博的转发和点评列表",
										 supportMethod:
											 QQWB._const.HTTP_METHOD_GET,
										 supportParams:
										 {
											 flag:
											 {
												 defaultValue:
													 "0",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 rootid:
											 {
												 defaultValue:
													 92035070199751,
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 pageflag:
											 {
												 defaultValue:
													 "1",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 pagetime:
											 {
												 defaultValue:
													 "0",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 reqnum:
											 {
												 defaultValue:
													 "2",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 twitterid:
											 {
												 defaultValue:
													 "0",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 }
										 }
									 },
									 "/t/comment":
									 {
										 category:
											 QQWB._const.API_CATEGORY_WEIBO,
										 description:
											 "点评一条微博",
										 supportMethod:
											 QQWB._const.HTTP_METHOD_POST,
										 supportParams:
										 {
											 content:
											 {
												 defaultValue:
													 "test",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 clientip:
											 {
												 defaultValue:
													 "127.0.0.1",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 jing:
											 {
												 defaultValue:
													 "",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 wei:
											 {
												 defaultValue:
													 "",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 reid:
											 {
												 defaultValue:
													 28135069067568,
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 }
										 }
									 },
									 "/t/add_music":
									 {
										 category:
											 QQWB._const.API_CATEGORY_WEIBO,
										 description:
											 "发表音频微博",
										 supportMethod:
											 QQWB._const.HTTP_METHOD_POST,
										 supportParams:
										 {
											 content:
											 {
												 defaultValue:
													 "test",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 clientip:
											 {
												 defaultValue:
													 "127.0.0.1",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 jing:
											 {
												 defaultValue:
													 "",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 wei:
											 {
												 defaultValue:
													 "",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 url:
											 {
												 defaultValue:
													 "http://url.cn",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 title:
											 {
												 defaultValue:
													 "歌名",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 author:
											 {
												 defaultValue:
													 "演唱者",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 reid:
											 {
												 defaultValue:
													 12345678,
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 }
										 }
									 },
									 "/t/add_video":
									 {
										 category:
											 QQWB._const.API_CATEGORY_WEIBO,
										 description:
											 "发表视频微博",
										 supportMethod:
											 QQWB._const.HTTP_METHOD_POST,
										 supportParams:
										 {
											 content:
											 {
												 defaultValue:
													 "test",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 clientip:
											 {
												 defaultValue:
													 "127.0.0.1",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 jing:
											 {
												 defaultValue:
													 "",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 wei:
											 {
												 defaultValue:
													 "",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 url:
											 {
												 defaultValue:
													 "http://url.cn",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 }
										 }
									 },
									 "/t/add_video":
									 {
										 category:
											 QQWB._const.API_CATEGORY_WEIBO,
										 description:
											 "发表视频微博",
										 supportMethod:
											 QQWB._const.HTTP_METHOD_POST,
										 supportParams:
										 {
											 content:
											 {
												 defaultValue:
													 "test",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 clientip:
											 {
												 defaultValue:
													 "127.0.0.1",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 jing:
											 {
												 defaultValue:
													 "",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 wei:
											 {
												 defaultValue:
													 "",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 url:
											 {
												 defaultValue:
													 "http://url.cn",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 }
										 }
									 },
									 "/t/getvideoinfo":
									 {
										 category:
											 QQWB._const.API_CATEGORY_WEIBO,
										 description:
											 "获取视频信息",
										 supportMethod:
											 QQWB._const.HTTP_METHOD_POST,
										 supportParams:
										 {
											 url:
											 {
												 defaultValue:
													 "http://v.youku.com/v_show/id_XMjExODczODM2.html",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 }
										 }
									 },
									 "/t/list":
									 {
										 category:
											 QQWB._const.API_CATEGORY_WEIBO,
										 description:
											 "根据微博ID批量得到微博数据",
										 supportMethod:
											 QQWB._const.HTTP_METHOD_GET,
										 supportParams:
										 {
											 ids:
											 {
												 defaultValue:
													 "39110101242147,39578069128701",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 }
										 }
									 },
									 "/t/add_video_prev":
									 {
										 category:
											 QQWB._const.API_CATEGORY_WEIBO,
										 description:
											 "预发表一条视频微博",
										 supportMethod:
											 QQWB._const.HTTP_METHOD_GET,
										 supportParams:
										 {
											 content:
											 {
												 defaultValue:
													 "test",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 clientip:
											 {
												 defaultValue:
													 "127.0.0.1",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 jing:
											 {
												 defaultValue:
													 "",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 wei:
											 {
												 defaultValue:
													 "",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 vid:
											 {
												 defaultValue:
													 "",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 title:
											 {
												 defaultValue:
													 "",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 }
										 }
									 },
									 "/t/sub_re_count":
									 {
										 category:
											 QQWB._const.API_CATEGORY_WEIBO,
										 description:
											 "获取转播的再次转播数",
										 supportMethod:
											 QQWB._const.HTTP_METHOD_GET,
										 supportParams:
										 {
											 ids:
											 {
												 defaultValue:
													 "8171051658365,55054116813124",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 }
										 }
									 },
									 "/user/info":
									 {
										 category:
											 QQWB._const.API_CATEGORY_ACCOUNT,
										 description:
											 "获取自己的详细资料",
										 supportMethod:
											 QQWB._const.HTTP_METHOD_GET
									 },
									 "/user/update":
									 {
										 category:
											 QQWB._const.API_CATEGORY_ACCOUNT,
										 description:
											 "更新个人资料",
										 supportMethod:
											 QQWB._const.HTTP_METHOD_POST,
										 supportParams:
										 {
											 nick:
											 {
												 defaultValue:
													 "",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 sex:
											 {
												 defaultValue:
													 "",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 year:
											 {
												 defaultValue:
													 2e3,
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 month:
											 {
												 defaultValue:
													 "1",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 day:
											 {
												 defaultValue:
													 "1",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 countrycode:
											 {
												 defaultValue:
													 "1",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 provincecode:
											 {
												 defaultValue:
													 11,
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 citycode:
											 {
												 defaultValue:
													 "1",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 introduction:
											 {
												 defaultValue:
													 "xxxx",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 }
										 }
									 },
									 "/user/update_edu":
									 {
										 category:
											 QQWB._const.API_CATEGORY_ACCOUNT,
										 description:
											 "更新个人教育信息",
										 supportMethod:
											 QQWB._const.HTTP_METHOD_POST,
										 supportParams:
										 {
											 year:
											 {
												 defaultValue:
													 1995,
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 level:
											 {
												 defaultValue:
													 "1",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 schoolid:
											 {
												 defaultValue:
													 "1",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 field:
											 {
												 defaultValue:
													 "1",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 departmentid:
											 {
												 defaultValue:
													 "1",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 }
										 }
									 },
									 "/user/update_head":
									 {
										 category:
											 QQWB._const.API_CATEGORY_ACCOUNT,
										 description:
											 "更新个人资料头像",
										 supportMethod:
											 QQWB._const.HTTP_METHOD_POST,
										 supportParams:
										 {
											 pic:
											 {
												 defaultValue:
													 "",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 }
										 }
									 },
									 "/user/other_info":
									 {
										 category:
											 QQWB._const.API_CATEGORY_ACCOUNT,
										 description:
											 "获取其他人资料",
										 supportMethod:
											 QQWB._const.HTTP_METHOD_GET,
										 supportParams:
										 {
											 name:
											 {
												 defaultValue:
													 "t",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 }
										 }
									 },
									 "/user/infos":
									 {
										 category:
											 QQWB._const.API_CATEGORY_ACCOUNT,
										 description:
											 "多用户信息",
										 supportMethod:
											 QQWB._const.HTTP_METHOD_POST,
										 supportParams:
										 {
											 names:
											 {
												 defaultValue:
													 "t,api_weibo",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 }
										 }
									 },
									 "/user/verify":
									 {
										 category:
											 QQWB._const.API_CATEGORY_ACCOUNT,
										 description:
											 "验证账户是否合法（是否注册微博）",
										 supportMethod:
											 QQWB._const.HTTP_METHOD_POST,
										 supportParams:
										 {
											 name:
											 {
												 defaultValue:
													 "t",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 }
										 }
									 },
									 "/user/emotion":
									 {
										 category:
											 QQWB._const.API_CATEGORY_ACCOUNT,
										 description:
											 "获取心情微博",
										 supportMethod:
											 QQWB._const.HTTP_METHOD_POST,
										 supportParams:
										 {
											 name:
											 {
												 defaultValue:
													 "t",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 pageflag:
											 {
												 defaultValue:
													 "0",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 id:
											 {
												 defaultValue:
													 "0",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 timstamp:
											 {
												 defaultValue:
													 "0",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 type:
											 {
												 defaultValue:
													 "0",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 contenttype:
											 {
												 defaultValue:
													 "1",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 accesslevel:
											 {
												 defaultValue:
													 "0",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 emotiontype:
											 {
												 defaultValue:
													 "0xFFFFFFFF",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 reqnum:
											 {
												 defaultValue:
													 10,
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 }
										 }
									 },
									 "/friends/fanslist":
									 {
										 category:
											 QQWB._const.API_CATEGORY_RELATION,
										 description:
											 "我的听众列表",
										 supportMethod:
											 QQWB._const.HTTP_METHOD_GET,
										 supportParams:
										 {
											 reqnum:
											 {
												 defaultValue:
													 20,
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 startindex:
											 {
												 defaultValue:
													 "0",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 }
										 }
									 },
									 "/friends/idollist":
									 {
										 category:
											 QQWB._const.API_CATEGORY_RELATION,
										 description:
											 "我收听的人列表",
										 supportMethod:
											 QQWB._const.HTTP_METHOD_GET,
										 supportParams:
										 {
											 reqnum:
											 {
												 defaultValue:
													 20,
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 startindex:
											 {
												 defaultValue:
													 "0",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 }
										 }
									 },
									 "/friends/blacklist":
									 {
										 category:
											 QQWB._const.API_CATEGORY_RELATION,
										 description:
											 "黑名单列表",
										 supportMethod:
											 QQWB._const.HTTP_METHOD_GET,
										 supportParams:
										 {
											 reqnum:
											 {
												 defaultValue:
													 20,
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 startindex:
											 {
												 defaultValue:
													 "0",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 }
										 }
									 },
									 "/friends/speciallist":
									 {
										 category:
											 QQWB._const.API_CATEGORY_RELATION,
										 description:
											 "特别收听列表",
										 supportMethod:
											 QQWB._const.HTTP_METHOD_GET,
										 supportParams:
										 {
											 reqnum:
											 {
												 defaultValue:
													 20,
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 startindex:
											 {
												 defaultValue:
													 "0",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 }
										 }
									 },
									 "/friends/add":
									 {
										 category:
											 QQWB._const.API_CATEGORY_RELATION,
										 description:
											 "收听某个用户",
										 supportMethod:
											 QQWB._const.HTTP_METHOD_POST,
										 supportParams:
										 {
											 name:
											 {
												 defaultValue:
													 "t",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 }
										 }
									 },
									 "/friends/del":
									 {
										 category:
											 QQWB._const.API_CATEGORY_RELATION,
										 description:
											 "取消收听某个用户",
										 supportMethod:
											 QQWB._const.HTTP_METHOD_POST,
										 supportParams:
										 {
											 name:
											 {
												 defaultValue:
													 "t",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 }
										 }
									 },
									 "/friends/addspecial":
									 {
										 category:
											 QQWB._const.API_CATEGORY_RELATION,
										 description:
											 "特别收听某个用户",
										 supportMethod:
											 QQWB._const.HTTP_METHOD_POST,
										 supportParams:
										 {
											 name:
											 {
												 defaultValue:
													 "t",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 }
										 }
									 },
									 "/friends/delspecial":
									 {
										 category:
											 QQWB._const.API_CATEGORY_RELATION,
										 description:
											 "取消特别收听某个用户",
										 supportMethod:
											 QQWB._const.HTTP_METHOD_POST,
										 supportParams:
										 {
											 name:
											 {
												 defaultValue:
													 "t",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 }
										 }
									 },
									 "/friends/addblacklist":
									 {
										 category:
											 QQWB._const.API_CATEGORY_RELATION,
										 description:
											 "添加某个用户到黑名单",
										 supportMethod:
											 QQWB._const.HTTP_METHOD_POST,
										 supportParams:
										 {
											 name:
											 {
												 defaultValue:
													 "t",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 }
										 }
									 },
									 "/friends/delblacklist":
									 {
										 category:
											 QQWB._const.API_CATEGORY_RELATION,
										 description:
											 "从黑名单中删除某个用户",
										 supportMethod:
											 QQWB._const.HTTP_METHOD_POST,
										 supportParams:
										 {
											 name:
											 {
												 defaultValue:
													 "t",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 }
										 }
									 },
									 "/friends/check":
									 {
										 category:
											 QQWB._const.API_CATEGORY_RELATION,
										 description:
											 "检查是否我的听众或收听的人",
										 supportMethod:
											 QQWB._const.HTTP_METHOD_GET,
										 supportParams:
										 {
											 names:
											 {
												 defaultValue:
													 "t",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 flag:
											 {
												 defaultValue:
													 "2",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 }
										 }
									 },
									 "/friends/user_fanslist":
									 {
										 category:
											 QQWB._const.API_CATEGORY_RELATION,
										 description:
											 "其他账户听众列表",
										 supportMethod:
											 QQWB._const.HTTP_METHOD_GET,
										 supportParams:
										 {
											 reqnum:
											 {
												 defaultValue:
													 30,
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 startindex:
											 {
												 defaultValue:
													 "0",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 name:
											 {
												 defaultValue:
													 "t",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 }
										 }
									 },
									 "/friends/user_idollist":
									 {
										 category:
											 QQWB._const.API_CATEGORY_RELATION,
										 description:
											 "其他账户收听的人列表",
										 supportMethod:
											 QQWB._const.HTTP_METHOD_GET,
										 supportParams:
										 {
											 reqnum:
											 {
												 defaultValue:
													 30,
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 startindex:
											 {
												 defaultValue:
													 "0",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 name:
											 {
												 defaultValue:
													 "t",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 }
										 }
									 },
									 "/friends/user_speciallist":
									 {
										 category:
											 QQWB._const.API_CATEGORY_RELATION,
										 description:
											 "其他账户特别收听的人列表",
										 supportMethod:
											 QQWB._const.HTTP_METHOD_GET,
										 supportParams:
										 {
											 reqnum:
											 {
												 defaultValue:
													 30,
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 startindex:
											 {
												 defaultValue:
													 "0",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 name:
											 {
												 defaultValue:
													 "",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 }
										 }
									 },
									 "/friends/fanslist_s":
									 {
										 category:
											 QQWB._const.API_CATEGORY_RELATION,
										 description:
											 "多听众列表",
										 supportMethod:
											 QQWB._const.HTTP_METHOD_GET,
										 supportParams:
										 {
											 reqnum:
											 {
												 defaultValue:
													 100,
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 startindex:
											 {
												 defaultValue:
													 "0",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 }
										 }
									 },
									 "/friends/idollist_s":
									 {
										 category:
											 QQWB._const.API_CATEGORY_RELATION,
										 description:
											 "多收听人列表",
										 supportMethod:
											 QQWB._const.HTTP_METHOD_GET,
										 supportParams:
										 {
											 reqnum:
											 {
												 defaultValue:
													 100,
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 startindex:
											 {
												 defaultValue:
													 "0",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 }
										 }
									 },
									 "/friends/mutual_list":
									 {
										 category:
											 QQWB._const.API_CATEGORY_RELATION,
										 description:
											 "互听关系链列表",
										 supportMethod:
											 QQWB._const.HTTP_METHOD_GET,
										 supportParams:
										 {
											 name:
											 {
												 defaultValue:
													 "t",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 startindex:
											 {
												 defaultValue:
													 "0",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 reqnum:
											 {
												 defaultValue:
													 30,
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 }
										 }
									 },
									 "/private/add":
									 {
										 category:
											 QQWB._const.API_CATEGORY_SIXIN,
										 description:
											 "发私信",
										 supportMethod:
											 QQWB._const.HTTP_METHOD_POST,
										 supportParams:
										 {
											 content:
											 {
												 defaultValue:
													 "test",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 clientip:
											 {
												 defaultValue:
													 "127.0.0.1",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 jing:
											 {
												 defaultValue:
													 "",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 wei:
											 {
												 defaultValue:
													 "",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 name:
											 {
												 defaultValue:
													 "mmplayer",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 }
										 }
									 },
									 "/private/del":
									 {
										 category:
											 QQWB._const.API_CATEGORY_SIXIN,
										 description:
											 "删除一条私信",
										 supportMethod:
											 QQWB._const.HTTP_METHOD_POST,
										 supportParams:
										 {
											 id:
											 {
												 defaultValue:
													 26154115313103,
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 }
										 }
									 },
									 "/private/recv":
									 {
										 category:
											 QQWB._const.API_CATEGORY_SIXIN,
										 description:
											 "收件箱",
										 supportMethod:
											 QQWB._const.HTTP_METHOD_GET,
										 supportParams:
										 {
											 pageflag:
											 {
												 defaultValue:
													 "0",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 pagetime:
											 {
												 defaultValue:
													 "0",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 reqnum:
											 {
												 defaultValue:
													 20,
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 lastid:
											 {
												 defaultValue:
													 "0",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 }
										 }
									 },
									 "/private/send":
									 {
										 category:
											 QQWB._const.API_CATEGORY_SIXIN,
										 description:
											 "发件箱",
										 supportMethod:
											 QQWB._const.HTTP_METHOD_GET,
										 supportParams:
										 {
											 pageflag:
											 {
												 defaultValue:
													 "0",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 pagetime:
											 {
												 defaultValue:
													 "0",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 reqnum:
											 {
												 defaultValue:
													 20,
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 lastid:
											 {
												 defaultValue:
													 "0",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 }
										 }
									 },
									 "/search/user":
									 {
										 category:
											 QQWB._const.API_CATEGORY_SEARCH,
										 description:
											 "搜索用户",
										 supportMethod:
											 QQWB._const.HTTP_METHOD_GET,
										 supportParams:
										 {
											 keyword:
											 {
												 defaultValue:
													 "test",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 pagesize:
											 {
												 defaultValue:
													 10,
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 page:
											 {
												 defaultValue:
													 "1",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 }
										 }
									 },
									 "/search/t":
									 {
										 category:
											 QQWB._const.API_CATEGORY_SEARCH,
										 description:
											 "搜索微博",
										 supportMethod:
											 QQWB._const.HTTP_METHOD_GET,
										 supportParams:
										 {
											 keyword:
											 {
												 defaultValue:
													 "test",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 pagesize:
											 {
												 defaultValue:
													 10,
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 page:
											 {
												 defaultValue:
													 "1",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 }
										 }
									 },
									 "/search/userbytag":
									 {
										 category:
											 QQWB._const.API_CATEGORY_SEARCH,
										 description:
											 "通过标签搜索用户",
										 supportMethod:
											 QQWB._const.HTTP_METHOD_GET,
										 supportParams:
										 {
											 keyword:
											 {
												 defaultValue:
													 "test",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 pagesize:
											 {
												 defaultValue:
													 10,
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 page:
											 {
												 defaultValue:
													 "1",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 }
										 }
									 },
									 "/trends/ht":
									 {
										 category:
											 QQWB._const.API_CATEGORY_TRENS,
										 description:
											 "话题热榜",
										 supportMethod:
											 QQWB._const.HTTP_METHOD_GET,
										 supportParams:
										 {
											 type:
											 {
												 defaultValue:
													 "3",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 reqnum:
											 {
												 defaultValue:
													 20,
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 pos:
											 {
												 defaultValue:
													 "0",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 }
										 }
									 },
									 "/trends/t":
									 {
										 category:
											 QQWB._const.API_CATEGORY_TRENS,
										 description:
											 "热门转播",
										 supportMethod:
											 QQWB._const.HTTP_METHOD_GET,
										 supportParams:
										 {
											 reqnum:
											 {
												 defaultValue:
													 20,
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 pos:
											 {
												 defaultValue:
													 "0",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 }
										 }
									 },
									 "/info/update":
									 {
										 category:
											 QQWB._const.API_CATEGORY_QUERY,
										 description:
											 "更新条数",
										 supportMethod:
											 QQWB._const.HTTP_METHOD_GET,
										 supportParams:
										 {
											 op:
											 {
												 defaultValue:
													 "0",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 type:
											 {
												 defaultValue:
													 "9",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 }
										 }
									 },
									 "/fav/addt":
									 {
										 category:
											 QQWB._const.API_CATEGORY_FAVORITE,
										 description:
											 "收藏一条微博",
										 supportMethod:
											 QQWB._const.HTTP_METHOD_POST,
										 supportParams:
										 {
											 id:
											 {
												 defaultValue:
													 123456789,
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 }
										 }
									 },
									 "/fav/delt":
									 {
										 category:
											 QQWB._const.API_CATEGORY_FAVORITE,
										 description:
											 "取消收藏一条微博",
										 supportMethod:
											 QQWB._const.HTTP_METHOD_POST,
										 supportParams:
										 {
											 id:
											 {
												 defaultValue:
													 123456789,
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 }
										 }
									 },
									 "/fav/list_t":
									 {
										 category:
											 QQWB._const.API_CATEGORY_FAVORITE,
										 description:
											 "收藏的微博列表",
										 supportMethod:
											 QQWB._const.HTTP_METHOD_GET,
										 supportParams:
										 {
											 pageflag:
											 {
												 defaultValue:
													 "0",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 nexttime:
											 {
												 defaultValue:
													 "0",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 prevtime:
											 {
												 defaultValue:
													 "0",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 reqnum:
											 {
												 defaultValue:
													 20,
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 lastid:
											 {
												 defaultValue:
													 "0",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 }
										 }
									 },
									 "/fav/addht":
									 {
										 category:
											 QQWB._const.API_CATEGORY_FAVORITE,
										 description:
											 "订阅话题",
										 supportMethod:
											 QQWB._const.HTTP_METHOD_POST,
										 supportParams:
										 {
											 id:
											 {
												 defaultValue:
													 123456789,
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 }
										 }
									 },
									 "/fav/delht":
									 {
										 category:
											 QQWB._const.API_CATEGORY_FAVORITE,
										 description:
											 "取消收藏话题",
										 supportMethod:
											 QQWB._const.HTTP_METHOD_POST,
										 supportParams:
										 {
											 id:
											 {
												 defaultValue:
													 123456789,
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 }
										 }
									 },
									 "/fav/list_ht":
									 {
										 category:
											 QQWB._const.API_CATEGORY_FAVORITE,
										 description:
											 "获取已订阅话题列表",
										 supportMethod:
											 QQWB._const.HTTP_METHOD_GET,
										 supportParams:
										 {
											 reqnum:
											 {
												 defaultValue:
													 20,
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 pageflag:
											 {
												 defaultValue:
													 "0",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 pagetime:
											 {
												 defaultValue:
													 "0",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 lastid:
											 {
												 defaultValue:
													 "0",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 }
										 }
									 },
									 "/ht/ids":
									 {
										 category:
											 QQWB._const.API_CATEGORY_TOPIC,
										 description:
											 "根据话题名称查询话题ID",
										 supportMethod:
											 QQWB._const.HTTP_METHOD_GET,
										 supportParams:
										 {
											 httexts:
											 {
												 defaultValue:
													 "abc,efg",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 }
										 }
									 },
									 "/ht/info":
									 {
										 category:
											 QQWB._const.API_CATEGORY_TOPIC,
										 description:
											 "根据话题ID获取话题相关微博",
										 supportMethod:
											 QQWB._const.HTTP_METHOD_GET,
										 supportParams:
										 {
											 ids:
											 {
												 defaultValue:
													 0xb04fd23c98500000,
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 }
										 }
									 },
									 "/tag/add":
									 {
										 category:
											 QQWB._const.API_CATEGORY_TAG,
										 description:
											 "添加标签",
										 supportMethod:
											 QQWB._const.HTTP_METHOD_POST,
										 supportParams:
										 {
											 tag:
											 {
												 defaultValue:
													 "snow",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 }
										 }
									 },
									 "/tag/del":
									 {
										 category:
											 QQWB._const.API_CATEGORY_TAG,
										 description:
											 "删除标签",
										 supportMethod:
											 QQWB._const.HTTP_METHOD_POST,
										 supportParams:
										 {
											 tagid:
											 {
												 defaultValue:
													 0x4735d427dffb9400,
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 }
										 }
									 },
									 "/other/kownperson":
									 {
										 category:
											 QQWB._const.API_CATEGORY_OTHER,
										 description:
											 "我可能认识的人",
										 supportMethod:
											 QQWB._const.HTTP_METHOD_GET,
										 supportParams:
										 {
											 ip:
											 {
												 defaultValue:
													 "127.0.0.1",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 country_code:
											 {
												 defaultValue:
													 "",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 province_code:
											 {
												 defaultValue:
													 "",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 },
											 city_code:
											 {
												 defaultValue:
													 "",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 }
										 }
									 },
									 "/other/kownperson":
									 {
										 category:
											 QQWB._const.API_CATEGORY_OTHER,
										 description:
											 "可能认识的人",
										 supportMethod:
											 QQWB._const.HTTP_METHOD_GET
									 },
									 "/other/shorturl":
									 {
										 category:
											 QQWB._const.API_CATEGORY_OTHER,
										 description:
											 "短URL转长URL",
										 supportMethod:
											 QQWB._const.HTTP_METHOD_GET,
										 supportParams:
										 {
											 url:
											 {
												 defaultValue:
													 "3M6GSa",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 }
										 }
									 },
									 "/other/get_emotions":
									 {
										 category:
											 QQWB._const.API_CATEGORY_OTHER,
										 description:
											 "获取表情接口",
										 supportMethod:
											 QQWB._const.HTTP_METHOD_GET,
										 supportParams:
										 {
											 type:
											 {
												 defaultValue:
													 "0",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 }
										 }
									 },
									 "/other/kownperson":
									 {
										 category:
											 QQWB._const.API_CATEGORY_OTHER,
										 description:
											 "我可能认识的人",
										 supportMethod:
											 QQWB._const.HTTP_METHOD_GET,
										 supportParams:
										 {
											 ip:
											 {
												 defaultValue:
													 "127.0.0.1",
												 description:
													 QQWB._const.API_NO_DESCRIPTION
											 }
										 }
									 },
									 "/other/videokey":
									 {
										 category:
											 QQWB._const.API_CATEGORY_OTHER,
										 description:
											 "获取视频上传的key",
										 supportMethod:
											 QQWB._const.HTTP_METHOD_GET
									 },
									 "/other/gettopreadd":
									 {
										 category:
											 QQWB._const.API_CATEGORY_OTHER,
										 description:
											 "一键转播热门排行",
										 supportMethod:
											 QQWB._const.HTTP_METHOD_GET
									 },
									 "/other/url_converge":
									 {
										 category:
											 QQWB._const.API_CATEGORY_OTHER,
										 description:
											 "短url聚合",
										 supportMethod:
											 QQWB._const.HTTP_METHOD_GET
									 } },
								 getDescriptor:
									 function(a)
									 {
										 return
											 this.apis[a]
									 },
								 isProvide:
									 function(a)
									 {
										 return
											 !!this.getDescriptor(a)
									 },
								 describe:
									 function(a)
									 { var
										 b
											 =
											 this.getDescriptor(a);
										 return
											 b
											 ?
											 b.category
											 +
											 ">"
											 +
											 b.description
											 :
											 ""
									 },
								 compat:
									 function(a)
									 {
										 return
											 !QQWB.String.startsWith(a,
													 "/")
											 &&
											 (a
											  =
											  "/"
											  +
											  a),
										 a.toLowerCase()
									 } });
var JSON; JSON || (JSON = {}), function() { function f(a) { return a < 10 ? "0"
	+ a : a }

    function quote(a) { return escapable.lastIndex = 0, escapable.test(a) ? '"'
	    + a.replace(escapable, function(a) { var b = meta[a]; return typeof
		    b == "string" ? b : "\\u" + ("0000" +
			    a.charCodeAt(0).toString(16)).slice(-4) }) + '"' :
		    '"' + a + '"' }

    function str(a, b) { var c, d, e, f, g = gap, h, i = b[a]; i && typeof i ==
	    "object" && typeof i.toJSON == "function" && (i = i.toJSON(a)),
	    typeof rep == "function" && (i = rep.call(b, a, i)); switch (typeof
			    i) { case "string": return quote(i); case "number":
				    return isFinite(i) ? String(i) : "null";
				    case "boolean": case "null": return
								 String(i);
				    case "object": if (!i) return "null"; gap
							   += indent, h = [];
						   if
							   (Object.prototype.toString.apply(i)
							    === "[object
							    Array]") { f =
								   i.length;
						   for (c = 0; c < f; c += 1)
							   h[c] = str(c, i) ||
								   "null";
						   return e = h.length === 0 ?
							   "[]" : gap ? "[\n" +
							   gap + h.join(",\n" +
									   gap)
							   + "\n" + g + "]" :
							   "[" + h.join(",") +
							   "]", gap = g, e } if
							   (rep && typeof rep
							    == "object") { f =
								    rep.length;
								    for (c = 0;
										    c
										    <
										    f;
										    c
										    +=
										    1)
									    typeof
										    rep[c]
										    ==
										    "string"
										    &&
										    (d
										     =
										     rep[c],
										     e
										     =
										     str(d,
											     i),
										     e
										     &&
										     h.push(quote(d)
											     +
											     (gap
											      ?
											      ":
											      "
											      :
											      ":")
											     +
											     e))
							    } else for (d in i)
								    Object.prototype.hasOwnProperty.call(i,
										    d)
									    &&
									    (e
									     =
									     str(d,
										     i),
									     e
									     &&
									     h.push(quote(d)
										     +
										     (gap
										      ?
										      ":
										      "
										      :
										      ":")
										     +
										     e));
							    return e = h.length
								    === 0 ?
								    "{}" : gap
								    ? "{\n" +
								    gap +
								    h.join(",\n"
										    +
										    gap)
								    + "\n" + g
								    + "}" : "{"
								    +
								    h.join(",")
								    + "}", gap
								    = g, e } }
    "use strict", typeof Date.prototype.toJSON != "function" &&
	    (Date.prototype.toJSON = function(a) { return
		    isFinite(this.valueOf()) ? this.getUTCFullYear() + "-" +
		    f(this.getUTCMonth() + 1) + "-" + f(this.getUTCDate()) +
		    "T" + f(this.getUTCHours()) + ":" + f(this.getUTCMinutes())
		    + ":" + f(this.getUTCSeconds()) + "Z" : null },
		    String.prototype.toJSON = Number.prototype.toJSON =
		    Boolean.prototype.toJSON = function(a) { return
			    this.valueOf() }); var cx =
	    /[\u0000\u00ad\u0600-\u0604\u070f\u17b4\u17b5\u200c-\u200f\u2028-\u202f\u2060-\u206f\ufeff\ufff0-\uffff]/g,
	    escapable =
		    /[\\\"\x00-\x1f\x7f-\x9f\u00ad\u0600-\u0604\u070f\u17b4\u17b5\u200c-\u200f\u2028-\u202f\u2060-\u206f\ufeff\ufff0-\uffff]/g,
	    gap, indent, meta = { "\b": "\\b", "\t": "\\t", "\n": "\\n", "\f":
		    "\\f", "\r": "\\r", '"': '\\"', "\\": "\\\\" }, rep; typeof
		    JSON.stringify != "function" && (JSON.stringify =
				    function(a, b, c) { var d; gap = "", indent
					    = ""; if (typeof c == "number") for
			    (d = 0; d < c; d += 1) indent += " "; else typeof c
			    == "string" && (indent = c); rep = b; if (!b ||
				    typeof b == "function" || typeof b ==
				    "object" && typeof b.length == "number")
			    return str("", { "": a }); throw new
			    Error("JSON.stringify") }), typeof JSON.parse !=
		    "function" && (JSON.parse = function(text, reviver) {
			    function walk(a, b) { var c, d, e = a[b]; if (e &&
				    typeof e == "object") for (c in e)
				    Object.prototype.hasOwnProperty.call(e, c)
			    && (d = walk(e, c), d !== undefined ? e[c] = d :
				    delete e[c]); return reviver.call(a, b, e)
			    } var j; text = String(text), cx.lastIndex = 0,
		    cx.test(text) && (text = text.replace(cx, function(a) {
			    return "\\u" + ("0000" +
				    a.charCodeAt(0).toString(16)).slice(-4)
		    })); if
		    (/^[\],:{}\s]*$/.test(text.replace(/\\(?:["\\\/bfnrt]|u[0-9a-fA-F]{4})/g,
						       "@").replace(/"[^"\\\n\r]*"|true|false|null|-?\d+(?:\.\d*)?(?:[eE][+\-]?\d+)?/g,
							       "]").replace(/(?:^|:|,)(?:\s*\[)+/g,
								       "")))
			    return j = eval("(" + text + ")"), typeof reviver
			    == "function" ? walk({ "": j }, "") : j; throw new
			    SyntaxError("JSON.parse") }) }(),
		    QQWB.extend("JSON", { fromString: function(a) { if (!a ||
					    !QQWB.String.isString(a)) return
		    {}; a = a.replace(/^\s+/, "").replace(/\s+$/, ""); if
		    (window.JSON && window.JSON.parse) a =
			    window.JSON.parse(a); else if
			    (/^[\],:{}\s]*$/.test(a.replace(/\\(?:["\\\/bfnrt]|u[0-9a-fA-F]{4})/g,
							    "@").replace(/"[^"\\\n\r]*"|true|false|null|-?\d+(?:\.\d*)?(?:[eE][+\-]?\d+)?/g,
								    "]").replace(/(?:^|:|,)(?:\s*\[)+/g,
									    "")))
			    a = (new Function("return " + data))(); else throw
			    new SyntaxError("Invalid JSON: " + a); return a },
		    stringify: function(a) { return a == null ? "" :
			    window.JSON.stringify(a) }, toString: function(a) {
				    return QQWB.JSON.stringify(a) }, parse:
			    function(a) { return a == null ? {} :
				    window.JSON.parse(a) } }, !0),
		    QQWB.provide("man", function(a) { return a =
			    this._apiProvider.compat(a),
		    this._apiProvider.getDescriptor(a) ?
			    QQWB.JSON.stringify(this._apiProvider.getDescriptor(a))
			    : "no such api" }), QQWB.extend("Array", { isArray:
				    function(a) { return
					    Object.prototype.toString.call(a)
				    === "[object Array]" }, inArray:
				    function(a, b) { for (var c = 0, d =
					    a.length; c < d; c++) if (b ===
						    a[c]) return !0; return !1
				    }, fromString: function(a, b, c) { return
					    QQWB.String.isString(a) ? (b = b ||
						    "", c ? a.split(b, c) :
						    a.split(b)) : [] },
						    fromArguments: function(a,
							    b) { return typeof
								    a !=
				    "object" ? [] : b ?
				    Array.prototype.slice.call(a, b) :
				    Array.prototype.slice.call(a) }, toArray:
				    function(a) { return typeof a == "string" ?
					    a.split("") : typeof a == "object"
				    ? Array.prototype.slice.call(a, 0) :
				    this.toArray(a.toString()) }, each:
				    function(a, b) { for (var c = 0, d =
						    a.length; c < d; c++) if
					    (!1 === b(c, a[c])) break }, get:
					    function(a, b) { var c = a.length;
						    if (Math.abs(b) < c) return
							    b >= 0 ? a[b] : a[c
								    + b] } }),
								    QQWB.extend("dom",
										    {
											    create:
									    function(a,
										    b)
									    {
										    var
									    c =
									    document.createElement(a
										    +
										    "");
								    if (b && c)
									    for
									    (attr
									     in
									     b)
									    b.hasOwnProperty(attr)
									    &&
									    (QQWB.String.startsWith(attr,
												    "data-")
									     ?
									     c.setAttribute(attr,
										     b[attr])
									     :
									     c[attr]
									     =
									     b[attr]);
								    return c },
								    createHidden:
									    function(a,
											    b,
											    c)
									    { a
										    =
											    a
											    ||
											    "div";
										    var
											    d
											    =
											    this.create(a,
													    b);
										    return
											    c
											    ?
											    (d.width
											     =
											     d.height
											     =
											     0,
											     d.style.width
											     =
											     d.style.height
											     =
											     0,
											     d.style.position
											     =
											     "absolute",
											     d.style.top
											     =
											     "-9999px")
											    :
											    d.style.display
											    =
											    "none",
											    d
									    },
								    append:
									    function(a,
											    b)
									    {
										    return
											    b
											    =
											    b
											    ||
											    document.body,
										    a
											    &&
											    a.nodeType
											    &&
											    b.appendChild(a),
										    this
									    },
								    html:
									    function(a,
											    b)
									    {
										    return
											    a
											    &&
											    a.nodeType
											    &&
											    b
											    &&
											    (a.innerHTML
											     =
											     b),
										    this
									    },
								    appendHidden:
									    function(a,
											    b,
											    c)
									    {
										    var
											    d
											    =
											    this.createHidden(null,
													    b,
													    c);
										    return
											    this.html(d,
													    a),
											    this.append(d)
									    },
								    remove:
									    function(a)
									    {
										    return
											    a
											    &&
											    a.nodeType
											    &&
											    a.parentNode
											    &&
											    a.parentNode.removeChild(a),
										    this
									    },
								    hasClass:
									    function(a,
											    b)
									    {
										    return
											    ("
											     "
											     +
											     a.className
											     +
											     "
											     ").indexOf("
											     "
											     +
											     b
											     +
											     "
											     ")
											    >=
											    0
									    },
								    addClass:
									    function(a,
											    b)
									    {
										    return
											    b
											    =
											    QQWB.String.trim(b),
										    QQWB.Array.isArray(a)
											    ?
											    (QQWB.Array.each(a,
													     function(a,
														     c)
													     {
														     QQWB.dom.addClass(c,
															     b)
													     }),
											     this)
											    :
												    (QQWB.dom.hasClass(a,
														       b)
												     ||
												     (a.className
												      =
												      a.className
												      +
												      "
												      "
												      +
												      b),
												     this)
									    },
								    removeClass:
									    function(a,
											    b)
									    {
										    return
											    b
											    =
											    QQWB.String.trim(b),
										    QQWB.Array.isArray(a)
											    ?
											    (QQWB.Array.each(a,
													     function(a,
														     c)
													     {
														     QQWB.dom.removeClass(c,
															     b)
													     }),
											     this)
											    :
												    (QQWB.dom.hasClass(a,
														       b)
												     &&
												     (a.className
												      =
												      a.className.replace(b,
													      ""),
												      QQWB.dom.removeClass(a,
													      b)),
												     this)
									    }
										    }),
										    QQWB.extend("queryString",
												    {
													    encode:
											    function(a,
												    b,
												    c,
												    d)
											    {
												    var
											    e
											    =
											    /%20/g,
										    f
											    =
											    b
											    ||
											    "&",
										    g
											    =
											    c
											    ||
											    encodeURIComponent,
										    h
											    =
											    [],
										    i
											    =
											    [],
										    j,
										    k;
										    for
											    (j
											     in
											     a)
												    if
													    (a.hasOwnProperty(j))
													    {
														    k
															    =
															    a[j];
														    if
															    (k
															     !==
															     null
															     &&
															     typeof
															     k
															     !=
															     "undefined")
															    {
																    j
																	    =
																	    g(j).replace(e,
																			    "+"),
																    k
																	    =
																	    g(k).replace(e,
																			    "+");
																    if
																	    (!d)
																		    h.push(j
																				    +
																				    "="
																				    +
																				    k);
																    else
																	    for
																		    (var
																		     l
																		     =
																		     0,
																		     m
																		     =
																		     d.length;
																		     l
																		     <
																		     m;
																		     l++)
																			    d[l]
																			    ===
																			    j
																			    &&
																			    (h[l]
																			     =
																			     j
																			     +
																			     "="
																			     +
																			     k)
															    }
													    }
										    for
											    (var
											     n
											     =
											     0,
											     o
											     =
											     h.length;
											     n
											     <
											     o;
											     n++)
												    typeof
												    h[n]
												    !=
												    "undefined"
												    &&
												    i.push(h[n]);
										    return
											    h
											    =
											    i,
											    i
												    =
												    null,
											    h.join(f)
											    },
										    decode:
											    function(a,
													    b,
													    c)
											    {
												    var
													    d
													    =
													    c
													    ||
													    decodeURIComponent,
												    e
													    =
													    b
													    ||
													    "&",
												    f
													    =
													    a.split(e),
												    g
													    =
													    {},
												    h;
												    for
													    (var
													     i
													     =
													     0,
													     j
													     =
													     f.length;
													     i
													     <
													     j;
													     i++)
														    h
														    =
														    f[i].split("=",
																    2),
														    h
															    &&
															    h[0]
															    &&
															    (g[d(h[0])]
															     =
															     d(h[1]));
												    return
													    g
											    }
												    }),
												    function()
{ function q(a, b, c, d, e, f) { for (var g = 0, h = d.length; g < h; g++) {
	var i = d[g]; if (i) { var j = !1; i = i[a]; while (i) { if (i.sizcache
			=== c) { j = d[i.sizset]; break } i.nodeType === 1 &&
		!f && (i.sizcache = c, i.sizset = g); if
		(i.nodeName.toLowerCase() === b) { j = i; break } i = i[a] }
	d[g] = j } } }

    function r(a, b, c, d, e, f) { for (var g = 0, i = d.length; g < i; g++) {
	    var j = d[g]; if (j) { var k = !1; j = j[a]; while (j) { if
		    (j.sizcache === c) { k = d[j.sizset]; break } if
			    (j.nodeType === 1) { f || (j.sizcache = c, j.sizset
					    = g); if (typeof b != "string") {
						    if (j === b) { k = !0;
							    break } } else if
								    (h.filter(b,
									      [j]).length
								     > 0) { k =
									     j;
									     break
								     } } j =
		    j[a] } d[g] = k } } } var a =
			    /((?:\((?:\([^()]+\)|[^()]+)+\)|\[(?:\[[^\[\]]*\]|['"][^'"]*['"]|[^\[\]'"]+)+\]|\\.|[^
						    >+~,(\[\\]+)+|[>+~])(\s*,\s*)?((?:.|\r|\n)*)/g,
					    b = 0, c =
					    Object.prototype.toString, d = !1,
					    e = !0, f = /\\/g, g = /\W/; [0,
					    0].sort(function() { return e = !1,
						    0 }); var h = function(b,
							    d, e, f) { e = e ||
								    [], d = d
				    || document; var g = d; if (d.nodeType !==
					    1 && d.nodeType !== 9) return [];
			    if (!b || typeof b != "string") return e; var k, l,
			    n, o, p, q, r, t, u = !0, v = h.isXML(d), w = [], x
				    = b; do { a.exec(""), k = a.exec(x); if (k)
					    { x = k[3], w.push(k[1]); if (k[2])
						    { o = k[3]; break } } }
			    while (k); if (w.length > 1 && j.exec(b)) if
				    (w.length === 2 && i.relative[w[0]]) l =
				    s(w[0] + w[1], d); else { l =
					    i.relative[w[0]] ? [d] :
				    h(w.shift(), d); while (w.length) b =
				    w.shift(), i.relative[b] && (b +=
						    w.shift()), l = s(b, l) }
			    else { !f && w.length > 1 && d.nodeType === 9 && !v
				    && i.match.ID.test(w[0]) &&
					    !i.match.ID.test(w[w.length - 1])
					    && (p = h.find(w.shift(), d, v), d
							    = p.expr ?
							    h.filter(p.expr,
								    p.set)[0] :
							    p.set[0]); if (d) {
								    p = f ? {
									    expr:
										    w.pop(),
									    set:
										    m(f)
								    } :
								    h.find(w.pop(),
										    w.length
										    ===
										    1
										    &&
										    (w[0]
										     ===
										     "~"
										     ||
										     w[0]
										     ===
										     "+")
										    &&
										    d.parentNode
										    ?
										    d.parentNode
										    :
										    d,
										    v),
									    l =
										    p.expr
										    ?
										    h.filter(p.expr,
												    p.set)
										    :
										    p.set,
									    w.length
										    >
										    0
										    ?
										    n
										    =
										    m(l)
										    :
										    u
										    =
										    !1;
								    while
									    (w.length)
										    q
										    =
										    w.pop(),
										    r
											    =
											    q,
										    i.relative[q]
											    ?
											    r
											    =
											    w.pop()
											    :
											    q
											    =
											    "",
										    r
											    ==
											    null
											    &&
											    (r
											     =
											     d),
										    i.relative[q](n,
												    r,
												    v)
							    } else n = w = [] }
			    n || (n = l), n || h.error(q || b); if (c.call(n)
					    === "[object Array]") if (!u)
				    e.push.apply(e, n); else if (d &&
						    d.nodeType === 1) for (t =
							    0; n[t] != null;
							    t++) n[t] && (n[t]
								    === !0 ||
								    n[t].nodeType
								    === 1 &&
								    h.contains(d,
									    n[t]))
								    &&
								    e.push(l[t]);
							    else for (t = 0;
									    n[t]
									    !=
									    null;
									    t++)
								    n[t] &&
									    n[t].nodeType
									    ===
									    1
									    &&
									    e.push(l[t]);
							    else m(n, e);
							    return o && (h(o,
										    g,
										    e,
										    f),
									    h.uniqueSort(e)),
								    e };
		    h.uniqueSort = function(a) { if (o) { d = e, a.sort(o); if
			    (d) for (var b = 1; b < a.length; b++) a[b] === a[b
				    - 1] && a.splice(b--, 1) } return a },
			    h.matches = function(a, b) { return h(a, null,
					    null, b) }, h.matchesSelector =
				    function(a, b) { return h(b, null, null,
						    [a]).length > 0 }, h.find =
				    function(a, b, c) { var d; if (!a) return
					    []; for (var e = 0, g =
							    i.order.length; e <
							    g; e++) { var h, j
								    =
									    i.order[e];
								    if (h =
										    i.leftMatch[j].exec(a))
								    { var k =
									    h[1];
									    h.splice(1,
											    1);
									    if
										    (k.substr(k.length
											      -
											      1)
										     !==
										     "\\")
										    {
											    h[1]
												    =
												    (h[1]
												     ||
												     "").replace(f,
													     ""),
											    d
												    =
												    i.find[j](h,
														    b,
														    c);
											    if
												    (d
												     !=
												     null)
												    {
													    a
														    =
														    a.replace(i.match[j],
																    "");
													    break
												    }
										    }
								    } } return
					    d || (d = typeof
							    b.getElementsByTagName
							    != "undefined" ?
							    b.getElementsByTagName("*")
							    : []), { set: d,
								    expr: a }
				    }, h.filter = function(a, b, c, d) { var e,
					    f, g = a, j = [], k = b, l = b &&
						    b[0] && h.isXML(b[0]);
					    while (a && b.length) { for (var m
							    in i.filter) if ((e
									    =
									    i.leftMatch[m].exec(a))
								    != null &&
								    e[2]) { var
									    n,
									    o,
									    p =
										    i.filter[m],
									    q =
										    e[1];
									    f =
										    !1,
										    e.splice(1,
												    1);
									    if
										    (q.substr(q.length
											      -
											      1)
										     ===
										     "\\")
											    continue;
									    k
										    ===
										    j
										    &&
										    (j
										     =
										     []);
									    if
										    (i.preFilter[m])
										    {
											    e
												    =
												    i.preFilter[m](e,
														    k,
														    c,
														    j,
														    d,
														    l);
											    if
												    (!e)
													    f
													    =
													    n
													    =
													    !0;
											    else
												    if
													    (e
													     ===
													     !0)
														    continue
										    }
									    if
										    (e)
											    for
												    (var
												     r
												     =
												     0;
												     (o
												      =
												      k[r])
												     !=
												     null;
												     r++)
													    if
														    (o)
														    {
															    n
																    =
																    p(o,
																		    e,
																		    r,
																		    k);
															    var
																    s
																    =
																    d
																    ^
																    !!
																    n;
															    c
																    &&
																    n
																    !=
																    null
																    ?
																    s
																    ?
																    f
																    =
																    !0
																    :
																    k[r]
																    =
																    !1
																    :
																    s
																    &&
																    (j.push(o),
																     f
																     =
																     !0)
														    }
									    if
										    (n
										     !==
										     undefined)
										    {
											    c
												    ||
												    (k
												     =
												     j),
											    a
												    =
												    a.replace(i.match[m],
														    "");
											    if
												    (!f)
													    return
													    [];
											    break
										    }
								    } if (a ===
										    g)
									    if
										    (f
										     ==
										     null)
											    h.error(a);
									    else
										    break;
								    g = a }
					    return k }, h.error = function(a) {
						    throw "Syntax error,
							  unrecognized
								  expression: "
								  + a }; var i
								  = h.selectors
								  = { order:
									  ["ID",
									  "NAME",
									  "TAG"],
									  match:
									  { ID:
										  /#((?:[\w\u00c0-\uFFFF\-]|\\.)+)/,
										  CLASS:
											  /\.((?:[\w\u00c0-\uFFFF\-]|\\.)+)/,
										  NAME:
											  /\[name=['"]*((?:[\w\u00c0-\uFFFF\-]|\\.)+)['"]*\]/,
										  ATTR:
											  /\[\s*((?:[\w\u00c0-\uFFFF\-]|\\.)+)\s*(?:(\S?=)\s*(?:(['"])(.*?)\3|(#?(?:[\w\u00c0-\uFFFF\-]|\\.)*)|)|)\s*\]/,
										  TAG:
											  /^((?:[\w\u00c0-\uFFFF\*\-]|\\.)+)/,
										  CHILD:
											  /:(only|nth|last|first)-child(?:\(\s*(even|odd|(?:[+\-]?\d+|(?:[+\-]?\d*)?n\s*(?:[+\-]\s*\d+)?))\s*\))?/,
										  POS:
											  /:(nth|eq|gt|lt|first|last|even|odd)(?:\((\d*)\))?(?=[^\-]|$)/,
										  PSEUDO:
											  /:((?:[\w\u00c0-\uFFFF\-]|\\.)+)(?:\((['"]?)((?:\([^\)]+\)|[^\(\)]*)+)\2\))?/
									  },
									  leftMatch:
									  {},
									  attrMap:
									  {
										  "class":
											  "className",
										  "for":
											  "htmlFor"
									  },
									  attrHandle:
									  {
										  href:
											  function(a)
											  {
												  return
													  a.getAttribute("href")
											  },
										  type:
											  function(a)
											  {
												  return
													  a.getAttribute("type")
											  }
									  },
									  relative:
									  {
										  "+":
											  function(a,
													  b)
											  {
												  var
													  c
													  =
													  typeof
													  b
													  ==
													  "string",
												  d
													  =
													  c
													  &&
													  !g.test(b),
												  e
													  =
													  c
													  &&
													  !d;
												  d
													  &&
													  (b
													   =
													   b.toLowerCase());
												  for
													  (var
													   f
													   =
													   0,
													   i
													   =
													   a.length,
													   j;
													   f
													   <
													   i;
													   f++)
														  if
															  (j
															   =
															   a[f])
															  {
																  while
																	  ((j
																	    =
																	    j.previousSibling)
																	   &&
																	   j.nodeType
																	   !==
																	   1);
																  a[f]
																	  =
																	  e
																	  ||
																	  j
																	  &&
																	  j.nodeName.toLowerCase()
																	  ===
																	  b
																	  ?
																	  j
																	  ||
																	  !1
																	  :
																	  j
																	  ===
																	  b
															  }
												  e
													  &&
													  h.filter(b,
															  a,
															  !0)
											  },
										  ">":
											  function(a,
													  b)
											  {
												  var
													  c,
												  d
													  =
													  typeof
													  b
													  ==
													  "string",
												  e
													  =
													  0,
												  f
													  =
													  a.length;
												  if
													  (d
													   &&
													   !g.test(b))
													  {
														  b
															  =
															  b.toLowerCase();
														  for
															  (;
															   e
															   <
															   f;
															   e++)
															  {
																  c
																	  =
																	  a[e];
																  if
																	  (c)
																	  {
																		  var
																			  i
																			  =
																			  c.parentNode;
																		  a[e]
																			  =
																			  i.nodeName.toLowerCase()
																			  ===
																			  b
																			  ?
																			  i
																			  :
																			  !1
																	  }
															  }
													  }
												  else
												  {
													  for
														  (;
														   e
														   <
														   f;
														   e++)
															  c
															  =
															  a[e],
															  c
																  &&
																  (a[e]
																   =
																   d
																   ?
																   c.parentNode
																   :
																   c.parentNode
																   ===
																   b);
													  d
														  &&
														  h.filter(b,
																  a,
																  !0)
												  }
											  },
										  "":
											  function(a,
													  c,
													  d)
											  {
												  var
													  e,
												  f
													  =
													  b++,
												  h
													  =
													  r;
												  typeof
													  c
													  ==
													  "string"
													  &&
													  !g.test(c)
													  &&
													  (c
													   =
													   c.toLowerCase(),
													   e
													   =
													   c,
													   h
													   =
													   q),
													  h("parentNode",
															  c,
															  f,
															  a,
															  e,
															  d)
											  },
										  "~":
											  function(a,
													  c,
													  d)
											  {
												  var
													  e,
												  f
													  =
													  b++,
												  h
													  =
													  r;
												  typeof
													  c
													  ==
													  "string"
													  &&
													  !g.test(c)
													  &&
													  (c
													   =
													   c.toLowerCase(),
													   e
													   =
													   c,
													   h
													   =
													   q),
													  h("previousSibling",
															  c,
															  f,
															  a,
															  e,
															  d)
											  }
									  },
									  find:
									  { ID:
										  function(a,
												  b,
												  c)
										  {
											  if
												  (typeof
												   b.getElementById
												   !=
												   "undefined"
												   &&
												   !c)
												  {
													  var
														  d
														  =
														  b.getElementById(a[1]);
													  return
														  d
														  &&
														  d.parentNode
														  ?
														  [d]
														  :
														  []
												  }
										  },
										  NAME:
											  function(a,
													  b)
											  {
												  if
													  (typeof
													   b.getElementsByName
													   !=
													   "undefined")
													  {
														  var
															  c
															  =
															  [],
														  d
															  =
															  b.getElementsByName(a[1]);
														  for
															  (var
															   e
															   =
															   0,
															   f
															   =
															   d.length;
															   e
															   <
															   f;
															   e++)
																  d[e].getAttribute("name")
																  ===
																  a[1]
																  &&
																  c.push(d[e]);
														  return
															  c.length
															  ===
															  0
															  ?
															  null
															  :
															  c
													  }
											  },
										  TAG:
											  function(a,
													  b)
											  {
												  if
													  (typeof
													   b.getElementsByTagName
													   !=
													   "undefined")
														  return
														  b.getElementsByTagName(a[1])
											  }
									  },
									  preFilter:
									  {
										  CLASS:
											  function(a,
													  b,
													  c,
													  d,
													  e,
													  g)
											  {
												  a
													  =
													  "
													  "
													  +
													  a[1].replace(f,
															  "")
													  +
													  "
													  ";
												  if
													  (g)
														  return
														  a;
												  for
													  (var
													   h
													   =
													   0,
													   i;
													   (i
													    =
													    b[h])
													   !=
													   null;
													   h++)
														  i
														  &&
														  (e
														   ^
														   (i.className
														    &&
														    ("
														     "
														     +
														     i.className
														     +
														     "
														     ").replace(/[\t\n\r]/g,
														  "
															  ").indexOf(a)
														    >=
														    0)
														   ?
														   c
														   ||
														   d.push(i)
														   :
														   c
														   &&
														   (b[h]
														    =
														    !1));
												  return
													  !1
											  },
										  ID:
											  function(a)
											  {
												  return
													  a[1].replace(f,
															  "")
											  },
										  TAG:
											  function(a,
													  b)
											  {
												  return
													  a[1].replace(f,
															  "").toLowerCase()
											  },
										  CHILD:
											  function(a)
											  {
												  if
													  (a[1]
													   ===
													   "nth")
													  {
														  a[2]
															  ||
															  h.error(a[0]),
														  a[2]
															  =
															  a[2].replace(/^\+|\s*/g,
																	  "");
														  var
															  c
															  =
															  /(-?)(\d*)(?:n([+\-]?\d*))?/.exec(a[2]
																	  ===
																	  "even"
																	  &&
																	  "2n"
																	  ||
																	  a[2]
																	  ===
																	  "odd"
																	  &&
																	  "2n+1"
																	  ||
																	  !/\D/.test(a[2])
																	  &&
																	  "0n+"
																	  +
																	  a[2]
																	  ||
																	  a[2]);
														  a[2]
															  =
															  c[1]
															  +
															  (c[2]
															   ||
															   1)
															  -
															  0,
															  a[3]
																  =
																  c[3]
																  -
																  0
													  }
												  else
													  a[2]
														  &&
														  h.error(a[0]);
												  return
													  a[0]
													  =
													  b++,
													  a
											  },
										  ATTR:
											  function(a,
													  b,
													  c,
													  d,
													  e,
													  g)
											  {
												  var
													  h
													  =
													  a[1]
													  =
													  a[1].replace(f,
															  "");
												  return
													  !g
													  &&
													  i.attrMap[h]
													  &&
													  (a[1]
													   =
													   i.attrMap[h]),
													  a[4]
														  =
														  (a[4]
														   ||
														   a[5]
														   ||
														   "").replace(f,
															   ""),
													  a[2]
														  ===
														  "~="
														  &&
														  (a[4]
														   =
														   "
														   "
														   +
														   a[4]
														   +
														   "
														   "),
													  a
											  },
										  PSEUDO:
											  function(b,
													  c,
													  d,
													  e,
													  f)
											  {
												  if
													  (b[1]
													   ===
													   "not")
														  if
															  ((a.exec(b[3])
															    ||
															    "").length
															   >
															   1
															   ||
															   /^\w/.test(b[3]))
																  b[3]
																  =
																  h(b[3],
																		  null,
																		  null,
																		  c);
														  else
														  {
															  var
																  g
																  =
																  h.filter(b[3],
																		  c,
																		  d,
																		  !0
																		  ^
																		  f);
															  return
																  d
																  ||
																  e.push.apply(e,
																		  g),
																  !1
														  }
												  else
													  if
														  (i.match.POS.test(b[0])
														   ||
														   i.match.CHILD.test(b[0]))
															  return
															  !0;
												  return
													  b
											  },
										  POS:
											  function(a)
											  {
												  return
													  a.unshift(!0),
												  a
											  }
									  },
									  filters:
									  {
										  enabled:
											  function(a)
											  {
												  return
													  a.disabled
													  ===
													  !1
													  &&
													  a.type
													  !==
													  "hidden"
											  },
										  disabled:
											  function(a)
											  {
												  return
													  a.disabled
													  ===
													  !0
											  },
										  checked:
											  function(a)
											  {
												  return
													  a.checked
													  ===
													  !0
											  },
										  selected:
											  function(a)
											  {
												  return
													  a.parentNode
													  &&
													  a.parentNode.selectedIndex,
												  a.selected
													  ===
													  !0
											  },
										  parent:
											  function(a)
											  {
												  return
													  !!a.firstChild
											  },
										  empty:
											  function(a)
											  {
												  return
													  !a.firstChild
											  },
										  has:
											  function(a,
													  b,
													  c)
											  {
												  return
													  !!h(c[3],
															  a).length
											  },
										  header:
											  function(a)
											  {
												  return
													  /h\d/i.test(a.nodeName)
											  },
										  text:
											  function(a)
											  {
												  var
													  b
													  =
													  a.getAttribute("type"),
												  c
													  =
													  a.type;
												  return
													  a.nodeName.toLowerCase()
													  ===
													  "input"
													  &&
													  "text"
													  ===
													  c
													  &&
													  (b
													   ===
													   c
													   ||
													   b
													   ===
													   null)
											  },
										  radio:
											  function(a)
											  {
												  return
													  a.nodeName.toLowerCase()
													  ===
													  "input"
													  &&
													  "radio"
													  ===
													  a.type
											  },
										  checkbox:
											  function(a)
											  {
												  return
													  a.nodeName.toLowerCase()
													  ===
													  "input"
													  &&
													  "checkbox"
													  ===
													  a.type
											  },
										  file:
											  function(a)
											  {
												  return
													  a.nodeName.toLowerCase()
													  ===
													  "input"
													  &&
													  "file"
													  ===
													  a.type
											  },
										  password:
											  function(a)
											  {
												  return
													  a.nodeName.toLowerCase()
													  ===
													  "input"
													  &&
													  "password"
													  ===
													  a.type
											  },
										  submit:
											  function(a)
											  {
												  var
													  b
													  =
													  a.nodeName.toLowerCase();
												  return
													  (b
													   ===
													   "input"
													   ||
													   b
													   ===
													   "button")
													  &&
													  "submit"
													  ===
													  a.type
											  },
										  image:
											  function(a)
											  {
												  return
													  a.nodeName.toLowerCase()
													  ===
													  "input"
													  &&
													  "image"
													  ===
													  a.type
											  },
										  reset:
											  function(a)
											  {
												  var
													  b
													  =
													  a.nodeName.toLowerCase();
												  return
													  (b
													   ===
													   "input"
													   ||
													   b
													   ===
													   "button")
													  &&
													  "reset"
													  ===
													  a.type
											  },
										  button:
											  function(a)
											  {
												  var
													  b
													  =
													  a.nodeName.toLowerCase();
												  return
													  b
													  ===
													  "input"
													  &&
													  "button"
													  ===
													  a.type
													  ||
													  b
													  ===
													  "button"
											  },
										  input:
											  function(a)
											  {
												  return
													  /input|select|textarea|button/i.test(a.nodeName)
											  },
										  focus:
											  function(a)
											  {
												  return
													  a
													  ===
													  a.ownerDocument.activeElement
											  }
									  },
									  setFilters:
									  {
										  first:
											  function(a,
													  b)
											  {
												  return
													  b
													  ===
													  0
											  },
										  last:
											  function(a,
													  b,
													  c,
													  d)
											  {
												  return
													  b
													  ===
													  d.length
													  -
													  1
											  },
										  even:
											  function(a,
													  b)
											  {
												  return
													  b
													  %
													  2
													  ===
													  0
											  },
										  odd:
											  function(a,
													  b)
											  {
												  return
													  b
													  %
													  2
													  ===
													  1
											  },
										  lt:
											  function(a,
													  b,
													  c)
											  {
												  return
													  b
													  <
													  c[3]
													  -
													  0
											  },
										  gt:
											  function(a,
													  b,
													  c)
											  {
												  return
													  b
													  >
													  c[3]
													  -
													  0
											  },
										  nth:
											  function(a,
													  b,
													  c)
											  {
												  return
													  c[3]
													  -
													  0
													  ===
													  b
											  },
										  eq:
											  function(a,
													  b,
													  c)
											  {
												  return
													  c[3]
													  -
													  0
													  ===
													  b
											  }
									  },
									  filter:
									  {
										  PSEUDO:
											  function(a,
													  b,
													  c,
													  d)
											  {
												  var
													  e
													  =
													  b[1],
												  f
													  =
													  i.filters[e];
												  if
													  (f)
														  return
														  f(a,
																  c,
																  b,
																  d);
												  if
													  (e
													   ===
													   "contains")
														  return
														  (a.textContent
														   ||
														   a.innerText
														   ||
														   h.getText([a])
														   ||
														   "").indexOf(b[3])
														  >=
														  0;
												  if
													  (e
													   ===
													   "not")
													  {
														  var
															  g
															  =
															  b[3];
														  for
															  (var
															   j
															   =
															   0,
															   k
															   =
															   g.length;
															   j
															   <
															   k;
															   j++)
																  if
																	  (g[j]
																	   ===
																	   a)
																		  return
																		  !1;
														  return
															  !0
													  }
												  h.error(e)
											  },
										  CHILD:
											  function(a,
													  b)
											  {
												  var
													  c
													  =
													  b[1],
												  d
													  =
													  a;
												  switch
													  (c)
													  {
														  case
															  "only":
														  case
														  "first":
														  while
															  (d
															   =
															   d.previousSibling)
																  if
																	  (d.nodeType
																	   ===
																	   1)
																		  return
																		  !1;
														  if
															  (c
															   ===
															   "first")
																  return
																  !0;
														  d
															  =
															  a;
														  case
															  "last":
															  while
																  (d
																   =
																   d.nextSibling)
																	  if
																		  (d.nodeType
																		   ===
																		   1)
																			  return
																			  !1;
														  return
															  !0;
														  case
															  "nth":
															  var
															  e
															  =
															  b[2],
															  f
																  =
																  b[3];
														  if
															  (e
															   ===
															   1
															   &&
															   f
															   ===
															   0)
																  return
																  !0;
														  var
															  g
															  =
															  b[0],
															  h
																  =
																  a.parentNode;
														  if
															  (h
															   &&
															   (h.sizcache
															    !==
															    g
															    ||
															    !a.nodeIndex))
															  {
																  var
																	  i
																	  =
																	  0;
																  for
																	  (d
																	   =
																	   h.firstChild;
																	   d;
																	   d
																	   =
																	   d.nextSibling)
																		  d.nodeType
																		  ===
																		  1
																		  &&
																		  (d.nodeIndex
																		   =
																		   ++i);
																  h.sizcache
																	  =
																	  g
															  }
														  var
															  j
															  =
															  a.nodeIndex
															  -
															  f;
														  return
															  e
															  ===
															  0
															  ?
															  j
															  ===
															  0
															  :
															  j
															  %
															  e
															  ===
															  0
															  &&
															  j
															  /
															  e
															  >=
															  0
													  }
											  },
										  ID:
											  function(a,
													  b)
											  {
												  return
													  a.nodeType
													  ===
													  1
													  &&
													  a.getAttribute("id")
													  ===
													  b
											  },
										  TAG:
											  function(a,
													  b)
											  {
												  return
													  b
													  ===
													  "*"
													  &&
													  a.nodeType
													  ===
													  1
													  ||
													  a.nodeName.toLowerCase()
													  ===
													  b
											  },
										  CLASS:
											  function(a,
													  b)
											  {
												  return
													  ("
													   "
													   +
													   (a.className
													    ||
													    a.getAttribute("class"))
													   +
													   "
													   ").indexOf(b)
													   >
													   -1
											  },
										  ATTR:
											  function(a,
													  b)
											  {
												  var
													  c
													  =
													  b[1],
												  d
													  =
													  i.attrHandle[c]
													  ?
													  i.attrHandle[c](a)
													  :
													  a[c]
													  !=
													  null
													  ?
													  a[c]
													  :
													  a.getAttribute(c),
												  e
													  =
													  d
													  +
													  "",
												  f
													  =
													  b[2],
												  g
													  =
													  b[4];
												  return
													  d
													  ==
													  null
													  ?
													  f
													  ===
													  "!="
													  :
													  f
													  ===
													  "="
													  ?
													  e
													  ===
													  g
													  :
													  f
													  ===
													  "*="
													  ?
													  e.indexOf(g)
													  >=
													  0
													  :
													  f
													  ===
													  "~="
													  ?
													  ("
													   "
													   +
													   e
													   +
													   "
													   ").indexOf(g)
													   >=
													   0
													   :
													   g
													   ?
													   f
													   ===
													   "!="
													   ?
													   e
													   !==
													   g
													   :
													   f
													   ===
													   "^="
													   ?
													   e.indexOf(g)
													   ===
													   0
													   :
													   f
													   ===
													   "$="
													   ?
													   e.substr(e.length
															   -
															   g.length)
													   ===
													   g
													   :
													   f
													   ===
													   "|="
													   ?
													   e
													   ===
													   g
													   ||
													   e.substr(0,
															   g.length
															   +
															   1)
													   ===
													   g
													   +
													   "-"
													   :
													   !1
													   :
													   e
													   &&
													   d
													   !==
													   !1
											  },
										  POS:
											  function(a,
													  b,
													  c,
													  d)
											  {
												  var
													  e
													  =
													  b[2],
												  f
													  =
													  i.setFilters[e];
												  if
													  (f)
														  return
														  f(a,
																  c,
																  b,
																  d)
											  }
									  } },
							  j = i.match.POS, k =
								  function(a,
										  b)
								  { return "\\"
									  + (b
											  -
											  0
											  +
											  1)
								  }; for (var l
										  in
										  i.match)
								  i.match[l] =
								  new
								  RegExp(i.match[l].source
										  +
										  /(?![^\[]*\])(?![^\(]*\))/.source),
									  i.leftMatch[l]
										  =
										  new
										  RegExp(/(^(?:.|\r|\n)*?)/.source
												  +
												  i.match[l].source.replace(/\\(\d+)/g,
													  k));
									  var m
										  =
										  function(a,
												  b)
										  {
											  return
												  a
												  =
												  Array.prototype.slice.call(a,
														  0),
											  b
												  ?
												  (b.push.apply(b,
														a),
												   b)
												  :
												  a
										  };
									  try {
										  Array.prototype.slice.call(document.documentElement.childNodes,
												  0)[0].nodeType
									  }
									  catch
										  (n)
										  {
											  m
												  =
												  function(a,
														  b)
												  {
													  var
														  d
														  =
														  0,
													  e
														  =
														  b
														  ||
														  [];
													  if
														  (c.call(a)
														   ===
														   "[object
														   Array]")
															  Array.prototype.push.apply(e,
																	  a);
													  else
														  if
															  (typeof
															   a.length
															   ==
															   "number")
																  for
																	  (var
																	   f
																	   =
																	   a.length;
																	   d
																	   <
																	   f;
																	   d++)
																		  e.push(a[d]);
														  else
															  for
																  (;
																   a[d];
																   d++)
																	  e.push(a[d]);
													  return
														  e
												  }
										  }
									  var
										  o,
										  p;
									  document.documentElement.compareDocumentPosition
										  ?
										  o
										  =
										  function(a,
												  b)
										  {
											  return
												  a
												  ===
												  b
												  ?
												  (d
												   =
												   !0,
												   0)
												  :
												  !a.compareDocumentPosition
												  ||
												  !b.compareDocumentPosition
												  ?
												  a.compareDocumentPosition
												  ?
												  -1
												  :
												  1
												  :
												  a.compareDocumentPosition(b)
												  &
												  4
												  ?
												  -1
												  :
												  1
										  }
									  : (o
											  =
											  function(a,
												  b)
											  {
												  if
										  (a
										   ===
										   b)
										  return
										  d
										  =
										  !0,
									  0; if
										  (a.sourceIndex
										   &&
										   b.sourceIndex)
										  return
										  a.sourceIndex
										  -
										  b.sourceIndex;
									  var
										  c,
										  e,
										  f
											  =
											  [],
										  g
											  =
											  [],
										  h
											  =
											  a.parentNode,
										  i
											  =
											  b.parentNode,
										  j
											  =
											  h;
									  if (h
											  ===
											  i)
										  return
											  p(a,
													  b);
									  if
										  (!h)
											  return
											  -1;
									  if
										  (!i)
											  return
											  1;
									  while
										  (j)
											  f.unshift(j),
											  j
												  =
												  j.parentNode;
									  j =
										  i;
									  while
										  (j)
											  g.unshift(j),
											  j
												  =
												  j.parentNode;
									  c =
										  f.length,
										  e
											  =
											  g.length;
									  for
										  (var
										   k
										   =
										   0;
										   k
										   <
										   c
										   &&
										   k
										   <
										   e;
										   k++)
											  if
												  (f[k]
												   !==
												   g[k])
													  return
													  p(f[k],
															  g[k]);
									  return
										  k
										  ===
										  c
										  ?
										  p(a,
												  g[k],
												  -1)
										  :
										  p(f[k],
												  b,
												  1)
											  },
										  p
											  =
											  function(a,
													  b,
													  c)
											  {
												  if
													  (a
													   ===
													   b)
														  return
														  c;
												  var
													  d
													  =
													  a.nextSibling;
												  while
													  (d)
													  {
														  if
															  (d
															   ===
															   b)
																  return
																  -1;
														  d
															  =
															  d.nextSibling
													  }
												  return
													  1
											  }),
										  h.getText
											  =
											  function(a)
											  {
												  var
													  b
													  =
													  "",
												  c;
												  for
													  (var
													   d
													   =
													   0;
													   a[d];
													   d++)
														  c
														  =
														  a[d],
														  c.nodeType
															  ===
															  3
															  ||
															  c.nodeType
															  ===
															  4
															  ?
															  b
															  +=
															  c.nodeValue
															  :
															  c.nodeType
															  !==
															  8
															  &&
															  (b
															   +=
															   h.getText(c.childNodes));
												  return
													  b
											  },
										  function()
										  {
											  var
												  a
												  =
												  document.createElement("div"),
											  b
												  =
												  "script"
												  +
												  (new
												   Date).getTime(),
											  c
												  =
												  document.documentElement;
											  a.innerHTML
												  =
												  "<a
												  name='"
												  +
												  b
												  +
												  "'/>",
												  c.insertBefore(a,
														  c.firstChild),
												  document.getElementById(b)
													  &&
													  (i.find.ID
													   =
													   function(a,
														   b,
														   c)
													   {
														   if
														  (typeof
														   b.getElementById
														   !=
														   "undefined"
														   &&
														   !c)
														  {
															  var
														  d
														  =
														  b.getElementById(a[1]);
													  return
														  d
														  ?
														  d.id
														  ===
														  a[1]
														  ||
														  typeof
														  d.getAttributeNode
														  !=
														  "undefined"
														  &&
														  d.getAttributeNode("id").nodeValue
														  ===
														  a[1]
														  ?
														  [d]
														  :
														  undefined
														  :
														  []
														  }
													   },
												  i.filter.ID
													  =
													  function(a,
															  b)
													  {
														  var
															  c
															  =
															  typeof
															  a.getAttributeNode
															  !=
															  "undefined"
															  &&
															  a.getAttributeNode("id");
														  return
															  a.nodeType
															  ===
															  1
															  &&
															  c
															  &&
															  c.nodeValue
															  ===
															  b
													  }),
												  c.removeChild(a),
												  c
													  =
													  a
													  =
													  null
										  }(),
										  function()
										  {
											  var
												  a
												  =
												  document.createElement("div");
											  a.appendChild(document.createComment("")),
												  a.getElementsByTagName("*").length
													  >
													  0
													  &&
													  (i.find.TAG
													   =
													   function(a,
														   b)
													   {
														   var
														  c
														  =
														  b.getElementsByTagName(a[1]);
													  if
														  (a[1]
														   ===
														   "*")
														  {
															  var
														  d
														  =
														  [];
													  for
														  (var
														   e
														   =
														   0;
														   c[e];
														   e++)
															  c[e].nodeType
															  ===
															  1
															  &&
															  d.push(c[e]);
													  c
														  =
														  d
														  }
													  return
														  c
													   }),
												  a.innerHTML
													  =
													  "<a
													  href='#'></a>",
												  a.firstChild
													  &&
													  typeof
													  a.firstChild.getAttribute
													  !=
													  "undefined"
													  &&
													  a.firstChild.getAttribute("href")
													  !==
													  "#"
													  &&
													  (i.attrHandle.href
													   =
													   function(a)
													   {
														   return
														  a.getAttribute("href",
															  2)
													   }),
												  a
													  =
													  null
										  }(),
										  document.querySelectorAll
											  &&
											  function()
											  {
												  var
													  a
													  =
													  h,
												  b
													  =
													  document.createElement("div"),
												  c
													  =
													  "__sizzle__";
												  b.innerHTML
													  =
													  "<p
													  class='TEST'></p>";
												  if
													  (b.querySelectorAll
													   &&
													   b.querySelectorAll(".TEST").length
													   ===
													   0)
														  return;
												  h
													  =
													  function(b,
															  d,
															  e,
															  f)
													  {
														  d
															  =
															  d
															  ||
															  document;
														  if
															  (!f
															   &&
															   !h.isXML(d))
															  {
																  var
																	  g
																	  =
																	  /^(\w+$)|^\.([\w\-]+$)|^#([\w\-]+$)/.exec(b);
																  if
																	  (g
																	   &&
																	   (d.nodeType
																	    ===
																	    1
																	    ||
																	    d.nodeType
																	    ===
																	    9))
																	  {
																		  if
																			  (g[1])
																				  return
																				  m(d.getElementsByTagName(b),
																						  e);
																		  if
																			  (g[2]
																			   &&
																			   i.find.CLASS
																			   &&
																			   d.getElementsByClassName)
																				  return
																				  m(d.getElementsByClassName(g[2]),
																						  e)
																	  }
																  if
																	  (d.nodeType
																	   ===
																	   9)
																	  {
																		  if
																			  (b
																			   ===
																			   "body"
																			   &&
																			   d.body)
																				  return
																				  m([d.body],
																						  e);
																		  if
																			  (g
																			   &&
																			   g[3])
																			  {
																				  var
																					  j
																					  =
																					  d.getElementById(g[3]);
																				  if
																					  (!j
																					   ||
																					   !j.parentNode)
																						  return
																						  m([],
																								  e);
																				  if
																					  (j.id
																					   ===
																					   g[3])
																						  return
																						  m([j],
																								  e)
																			  }
																		  try
																		  {
																			  return
																				  m(d.querySelectorAll(b),
																						  e)
																		  }
																		  catch
																			  (k)
																			  {}
																	  }
																  else
																	  if
																		  (d.nodeType
																		   ===
																		   1
																		   &&
																		   d.nodeName.toLowerCase()
																		   !==
																		   "object")
																		  {
																			  var
																				  l
																				  =
																				  d,
																			  n
																				  =
																				  d.getAttribute("id"),
																			  o
																				  =
																				  n
																				  ||
																				  c,
																			  p
																				  =
																				  d.parentNode,
																			  q
																				  =
																				  /^\s*[+~]/.test(b);
																			  n
																				  ?
																				  o
																				  =
																				  o.replace(/'/g,
																						  "\\$&")
																				  :
																				  d.setAttribute("id",
																						  o),
																				  q
																					  &&
																					  p
																					  &&
																					  (d
																					   =
																					   d.parentNode);
																			  try
																			  {
																				  if
																					  (!q
																					   ||
																					   p)
																						  return
																						  m(d.querySelectorAll("[id='"
																									  +
																									  o
																									  +
																									  "']
																									  "
																									  +
																									  b),
																								  e)
																			  }
																			  catch
																				  (r)
																				  {}
																			  finally
																			  {
																				  n
																					  ||
																					  l.removeAttribute("id")
																			  }
																		  }
															  }
														  return
															  a(b,
																	  d,
																	  e,
																	  f)
													  };
												  for
													  (var
													   d
													   in
													   a)
														  h[d]
														  =
														  a[d];
												  b
													  =
													  null
											  }(),
										  function()
										  {
											  var
												  a
												  =
												  document.documentElement,
											  b
												  =
												  a.matchesSelector
												  ||
												  a.mozMatchesSelector
												  ||
												  a.webkitMatchesSelector
												  ||
												  a.msMatchesSelector;
											  if
												  (b)
												  {
													  var
														  c
														  =
														  !b.call(document.createElement("div"),
																  "div"),
														  d
															  =
															  !1;
													  try
													  {
														  b.call(document.documentElement,
																  "[test!='']:sizzle")
													  }
													  catch
														  (e)
														  {
															  d
																  =
																  !0
														  }
													  h.matchesSelector
														  =
														  function(a,
																  e)
														  {
															  e
																  =
																  e.replace(/\=\s*([^'"\]]*)\s*\]/g,
																		  "='$1']");
															  if
																  (!h.isXML(a))
																	  try
																	  {
																		  if
																			  (d
																			   ||
																			   !i.match.PSEUDO.test(e)
																			   &&
																			   !/!=/.test(e))
																			  {
																				  var
																					  f
																					  =
																					  b.call(a,
																							  e);
																				  if
																					  (f
																					   ||
																					   !c
																					   ||
																					   a.document
																					   &&
																					   a.document.nodeType
																					   !==
																					   11)
																						  return
																						  f
																			  }
																	  }
															  catch
																  (g)
																  {}
															  return
																  h(e,
																		  null,
																		  null,
																		  [a]).length
																  >
																  0
														  }
												  }
										  }(),
										  function()
										  {
											  var
												  a
												  =
												  document.createElement("div");
											  a.innerHTML
												  =
												  "<div
												  class='test
												  e'></div><div
												  class='test'></div>";
											  if
												  (!a.getElementsByClassName
												   ||
												   a.getElementsByClassName("e").length
												   ===
												   0)
													  return;
											  a.lastChild.className
												  =
												  "e";
											  if
												  (a.getElementsByClassName("e").length
												   ===
												   1)
													  return;
											  i.order.splice(1,
													  0,
													  "CLASS"),
												  i.find.CLASS
													  =
													  function(a,
															  b,
															  c)
													  {
														  if
															  (typeof
															   b.getElementsByClassName
															   !=
															   "undefined"
															   &&
															   !c)
																  return
																  b.getElementsByClassName(a[1])
													  },
												  a
													  =
													  null
										  }(),
										  document.documentElement.contains
											  ?
											  h.contains
											  =
											  function(a,
													  b)
											  {
												  return
													  a
													  !==
													  b
													  &&
													  (a.contains
													   ?
													   a.contains(b)
													   :
													   !0)
											  }
									  :
										  document.documentElement.compareDocumentPosition
										  ?
										  h.contains
										  =
										  function(a,
												  b)
										  {
											  return
												  !!(a.compareDocumentPosition(b)
														  &
														  16)
										  }
									  :
										  h.contains
										  =
										  function()
										  {
											  return
												  !1
										  },
										  h.isXML
											  =
											  function(a)
											  {
												  var
													  b
													  =
													  (a
													   ?
													   a.ownerDocument
													   ||
													   a
													   :
													   0).documentElement;
												  return
													  b
													  ?
													  b.nodeName
													  !==
													  "HTML"
													  :
													  !1
											  };
									  var s
										  =
										  function(a,
												  b)
										  {
											  var
												  c,
											  d
												  =
												  [],
											  e
												  =
												  "",
											  f
												  =
												  b.nodeType
												  ?
												  [b]
												  :
												  b;
											  while
												  (c
												   =
												   i.match.PSEUDO.exec(a))
													  e
													  +=
													  c[0],
													  a
														  =
														  a.replace(i.match.PSEUDO,
																  "");
											  a
												  =
												  i.relative[a]
												  ?
												  a
												  +
												  "*"
												  :
												  a;
											  for
												  (var
												   g
												   =
												   0,
												   j
												   =
												   f.length;
												   g
												   <
												   j;
												   g++)
													  h(a,
															  f[g],
															  d);
											  return
												  h.filter(e,
														  d)
										  },
										  t
											  =
											  window.Sizzle;
									  h.noConflict
										  =
										  function()
										  {
											  window.Sizzle
												  =
												  t
										  },
										  window.Sizzle
											  =
											  h
}(), function() { function b(b) { var c; if (!QQWB.Array.isArray(this)) return
	this; c = []; for (var d = 0, e = this.length; d < e; d++) c =
		c.concat(a(b, this[d])); return c }

    function c(b) { return typeof b != "string" ? this : b.length <= 0 ? this :
	    a.matches(":contains(" + b + ")", this) }

    function d(b) { return a.matches(b, this) }

    function e(b) { return a.matches(":not(" + b + ")", this) }

    function f(a) { return QQWB.Array.get(this, a) }

    function g(a) { if (!QQWB.Array.isArray(this) || !a) return this; for (var
		    b = 0, c = this.length; b < c; b++) if (a(this[b]) === !1)
	    break; return this }

    function h(a) { return !a.find && (a.find = function(c) { return
	    h(b.call(a, c)) }), !a.contains && (a.contains = function(b) {
		    return h(c.call(a, b)) }), !a.keep && (a.keep = function(b)
		    { return h(d.call(a, b)) }), !a.tear && (a.tear =
		    function(b) { return h(e.call(a, b)) }), !a.get && (a.get =
		    function(b) { return f.call(a, b) }), !a.each && (a.each =
		    function(b) { return g.call(a, b) }), a } var a =
		    window.Sizzle; a.noConflict(), QQWB.provide("dom.find",
				    function(b, c) { return h(a(b, c)) }),
	    QQWB._alias("find", QQWB.dom.find) }(), function() { var a =
		    function() { function b(a) { return
			    Object.prototype.toString.call(a).slice(8,
					    -1).toLowerCase() }

	    function c(a, b) { for (var c = []; b > 0; c[--b] = a); return
		    c.join("") } var d = function() { return
			    d.cache.hasOwnProperty(arguments[0]) ||
				    (d.cache[arguments[0]] =
				     d.parse(arguments[0])),
			    d.format.call(null, d.cache[arguments[0]],
					    arguments) }; return d.format =
				    function(d, e) { var f = 1, g = d.length, h
					    = "", i, j = [], k, l, m, n, o, p;
					    for (k = 0; k < g; k++) { h =
						    b(d[k]); if (h ===
								    "string")
							    j.push(d[k]); else
							    if (h === "array")
							    { m = d[k]; if
								    (m[2]) { i
									    =
										    e[f];
									    for
										    (l
										     =
										     0;
										     l
										     <
										     m[2].length;
										     l++)
										    {
											    if
												    (!i.hasOwnProperty(m[2][l]))
													    throw
													    a('[sprintf]
															    property
															    "%s"
															    does
															    not
															    exist',
															    m[2][l]);
											    i
												    =
												    i[m[2][l]]
										    }
								    } else m[1]
									    ? i
										    =
										    e[m[1]]
										    :
										    i
										    =
										    e[f++];
								    if
									    (/[^s]/.test(m[8])
									     &&
									     b(i)
									     !=
									     "number")
										    throw
										    a("[sprintf]
												    expecting
												    number
												    but
												    found
												    %s",
												    b(i));
								    switch
									    (m[8])
									    {
										    case
											    "b":
											    i
											    =
											    i.toString(2);
										    break;
										    case
											    "c":
											    i
											    =
											    String.fromCharCode(i);
										    break;
										    case
											    "d":
											    i
											    =
											    parseInt(i,
													    10);
										    break;
										    case
											    "e":
											    i
											    =
											    m[7]
											    ?
											    i.toExponential(m[7])
											    :
											    i.toExponential();
										    break;
										    case
											    "f":
											    i
											    =
											    m[7]
											    ?
											    parseFloat(i).toFixed(m[7])
											    :
											    parseFloat(i);
										    break;
										    case
											    "o":
											    i
											    =
											    i.toString(8);
										    break;
										    case
											    "s":
											    i
											    =
											    (i
											     =
											     i
											     ?
											     String(i)
											     :
											     "")
											    &&
											    m[7]
											    ?
											    i.substring(0,
													    m[7])
											    :
											    i;
										    break;
										    case
											    "u":
											    i
											    =
											    Math.abs(i);
										    break;
										    case
											    "x":
											    i
											    =
											    i.toString(16);
										    break;
										    case
											    "X":
											    i
											    =
											    i.toString(16).toUpperCase()
									    } i
								    =
									    /[def]/.test(m[8])
									    &&
									    m[3]
									    &&
									    i
									    >=
									    0 ?
									    "+"
									    + i
									    :
									    i,
									    o =
										    m[4]
										    ?
										    m[4]
										    ==
										    "0"
										    ?
										    "0"
										    :
										    m[4].charAt(1)
										    :
										    "
										    ",
									    p =
										    m[6]
										    -
										    String(i).length,
									    n =
										    m[6]
										    ?
										    c(o,
												    p)
										    :
										    "",
									    j.push(m[5]
											    ?
											    i
											    +
											    n
											    :
											    n
											    +
											    i)
							    } } return
					    j.join("") }, d.cache = {}, d.parse
						    = function(a) { var b = a,
							    c = [], d = [], e =
								    0; while
								    (b) { if
									    ((c
									      =
									      /^[^\x25]+/.exec(b))
									     !==
									     null)
										    d.push(c[0]);
									    else
										    if
											    ((c
											      =
											      /^\x25{2}/.exec(b))
											     !==
											     null)
												    d.push("%");
										    else
										    {
											    if
												    ((c
												      =
												      /^\x25(?:([1-9]\d*)\$|\(([^\)]+)\))?(\+)?(0|'[^$])?(-)?(\d+)?(?:\.(\d+))?([b-fosuxX])/.exec(b))
												     ===
												     null)
													    throw
													    "[sprintf]
													    huh?";
											    if
												    (c[2])
												    {
													    e
														    |=
														    1;
													    var
														    f
														    =
														    [],
														    g
															    =
															    c[2],
														    h
															    =
															    [];
													    if
														    ((h
														      =
														      /^([a-z_][a-z_\d]*)/i.exec(g))
														     ===
														     null)
															    throw
															    "[sprintf]
															    huh?";
													    f.push(h[1]);
													    while
														    ((g
														      =
														      g.substring(h[0].length))
														     !==
														     "")
															    if
																    ((h
																      =
																      /^\.([a-z_][a-z_\d]*)/i.exec(g))
																     !==
																     null)
																	    f.push(h[1]);
															    else
																    if
																	    ((h
																	      =
																	      /^\[(\d+)\]/.exec(g))
																	     !==
																	     null)
																		    f.push(h[1]);
																    else
																	    throw
																		    "[sprintf]
																		    huh?";
													    c[2]
														    =
														    f
												    }
											    else
												    e
													    |=
													    2;
											    if
												    (e
												     ===
												     3)
													    throw
													    "[sprintf]
													    mixing
													    positional
													    and
													    named
													    placeholders
													    is
													    not
													    (yet)
													    supported";
											    d.push(c)
										    }
									    b =
										    b.substring(c[0].length)
								    } return d
						    }, d }(), b = function(b,
								    c) { return
									    c.unshift(b),
									    a.apply(null,
											    c)
								    };
		    QQWB.extend("String.format", { sprintf: a, vsprintf: b }),
			    QQWB._alias("format", QQWB.String.format) }(),
			    QQWB.extend("time", { now: function() { return
				    +this.dateNow() }, secondsNow: function() {
					    return Math.round(this.now() / 1e3)
				    }, dateNow: function() { return new Date },
			    shortTime: function(a, b) { return a instanceof
				    Date || (b = a, a = this.dateNow()), b = b
				    || "%(year)s/%(month)s/%(day)s
				    %(hour)02d:%(minute)02d:%(second)02d",
			    QQWB.format.sprintf(b, { year: a.getFullYear(),
				    month: a.getMonth(), day: a.getDate(),
			    hour: a.getHours(), minute: a.getMinutes(), second:
				    a.getSeconds() }) } }), QQWB.extend("log",
			    { CRITICAL: 50, ERROR: 40, WARNING: 30, INFO: 20,
				    DEBUG: 10, NOTSET: 0, _level: 0, _format:
					    "%(source)s%(popup)s%(frame)s%(name)s:
					    [%(levelname)s] %(time)s
					    %(message)s", setLevel: function(a)
					    { return this._level = a, this },
				    setFormat: function(a) { return
					    this._format = a, this }, debug:
					    function(a) { return this.DEBUG >=
						    this._level &&
					    this._out("DEBUG", a), this },
				    info: function(a) { return this.INFO >=
					    this._level && this._out("INFO",
						    a), this }, warning:
					    function(a) { return this.WARNING
						    >= this._level &&
					    this._out("WARNING", a), this },
				    error: function(a) { return this.ERROR >=
					    this._level && this._out("ERROR",
						    a), this }, critical:
					    function(a) { return this.CRITICAL
						    >= this._level &&
							    this._out("CRITICAL",
									    a),
						    this }, _out: function(a,
								    b) { var c
									    =
										    this._format;
									    c =
										    QQWB.format.sprintf(c,
												    {
													    name:
											    QQWB.name,
										    levelname:
											    a,
										    time:
											    QQWB.time.shortTime(),
										    message:
											    b,
										    frame:
											    window
											    !=
											    window.parent
											    ?
											    "*"
											    :
											    "",
										    source:
											    window.name
											    ?
											    window.name
											    :
											    "",
										    popup:
											    window.opener
											    ||
											    window.name
											    ===
											    QQWB._const.AUTH_WINDOW_NAME
											    ?
											    "#"
											    :
											    ""
												    }),
												    this._capture
													    &&
													    typeof
													    this._captureLevel
													    ==
													    "number"
													    &&
													    this[a]
													    >
													    this._captureLevel
													    &&
													    this._capturedMessages
													    &&
													    (this._capturedMessages.length
													     >=
													     this._captureMaxSize
													     &&
													     this._capturedMessages.shift(),
													     this._capturedMessages.push(c)),
												    QQWB.debug
													    &&
													    window.console
													    &&
													    window.console.log(c)
								    },
								    startCapture:
									    function(a,
											    b)
									    {
										    return
											    this._captureLevel
											    =
											    a
											    ||
											    this.NOTSET,
										    this._captureMaxSize
											    =
											    b
											    ||
											    50,
										    this._capturedMessages
											    =
											    [],
										    this._capture
											    =
											    !0,
										    this
									    },
								    stopCapture:
									    function()
									    {
										    return
											    this._capture
											    &&
											    (this._capture
											     =
											     !1),
										    this
									    },
								    lastCaptured:
									    function(a)
									    {
										    return
											    a
											    =
											    a
											    ||
											    "\n",
										    this._capturedMessages
											    ?
											    this._capturedMessages.join(a)
											    :
											    ""
									    }
			    }), QQWB.extend("cookie", { set: function(a, b, c,
					    d, e, f) { f = f || escape; if
						    (typeof c == "undefined" ||
						     c === null) c = -1; var g
							    = e ? "domain=" + e
				    : "", h = d ? "path=" + d : "", i = "";
			    return c === 0 ? i = "expires=" + (new Date(1970,
					    1, 1)).toUTCString() : c > 0 && (i
					    = "expires=" + (new Date(+(new
								    Date) + c *
							    1e3)).toUTCString()),
					    document.cookie = [a + "=" + f(b),
			    i, h, g].join("; "), this }, get: function(a, b, c)
			    { b = b || unescape; var d = a + "="; cookies =
				    (document.cookie || "").split(/\s*;\s*/);
				    for (var e = 0, f = cookies.length; e < f;
					    e++) { var g = cookies[e]; if
						    (g.indexOf(d) === 0) return
				    b(g.substr(d.length)) } return c }, del:
				    function(a, b, c) { return this.set(a, "",
					    0, b, c), document.cookie.indexOf(a
						    + "=") >= 0 &&
						    QQWB.log.warning("Cookie
								    may not be
								    deleted as
								    you
								    expected"),
					    this } }), QQWB.extend("XML", {
						    isXML: function(a) {},
					    toString: function(a) { return
						    this.stringify(a) },
					    stringify: function(a) { var b;
						    return window.ActiveXObject
						    ? b = a.xml : b = (new
							    XMLSerializer).serializeToString(a),
					    b }, fromString: function(a) { var
						    b; if
						    (window.ActiveXObject) b =
						    new
						    ActiveXObject("Microsoft.XMLDOM"),
					    b.async = "false", b.loadXML(a);
					    else { var c = new DOMParser; b =
						    c.parseFromString(a,
							    "text/xml") }
					    return b } }, !0),
					    QQWB.extend("Function", {
						    isFunction: function(a) {
							    return typeof a ==
						    "function" } }),
					    QQWB.extend("deferred", {
						    _promiseMethods: "done fail
						    isResolved isRejected
						    promise then always success
						    error complete
						    whatever".split(" "),
					    _deferred: function() { var a = [],
						    b, c, d, e = { done:
							    function() { if
								    (!d) { var
									    c =
						    arguments, f, g; b && (g =
							    b, b = 0); for (var
								    h = 0, i =
								    c.length; h
								    < i; h++) f
							    = c[h],
					    QQWB.Array.isArray(f) ?
						    e.done.apply(e, f) :
						    QQWB.Function.isFunction(f)
						    && a.push(f); g &&
						    e.resolveWith(g[0], g[1]) }
								    return this
							    }, resolveWith:
							    function(e, f) { if
								    (!d && !b
								     && !c) { f
									     =
										     f
										     ||
										     [],
									     c
										     =
										     1;
									     try
									     {
										     while
											     (a[0])
												     a.shift().apply(e,
														     f)
									     }
									     finally
									     {
										     b
											     =
											     [e,
											     f],
											     c
												     =
												     0
									     }
								     } return
								    this },
								    resolve:
									    function()
									    {
										    return
											    e.resolveWith(this,
													    arguments),
										    this
									    },
								    isResolved:
									    function()
									    {
										    return
											    !!c
											    ||
											    !!
											    b
									    },
								    cancel:
									    function()
									    {
										    return
											    d
											    =
											    1,
										    a
											    =
											    [],
										    this
									    }
						    }; return e }, deferred:
					    function(a) { var b, c =
						    QQWB.deferred._deferred(),
						    d =
							    QQWB.deferred._deferred();
						    return QQWB.extend(c, {
							    fail: d.done, then:
							    function(a, b) {
								    return
							    c.done(a).fail(b),
							   this }, always:
							    function() { return
								    c.done.apply(c,
									    arguments).fail.apply(this,
										    arguments)
							    }, rejectWith:
						    d.resolveWith, reject:
							    d.resolve,
							   isRejected:
							    d.isResolved,
							   promise: function(a)
						    { if (a == null) { if (b)
									     return
							    b; b = a = {} } var
							    d =
							    QQWB.deferred._promiseMethods.length;
									     while
										     (d--)
											     a[QQWB.deferred._promiseMethods[d]]
											     =
											     c[QQWB.deferred._promiseMethods[d]];
									     return
										     a
						    } }), c.success = c.done,
							   c.error = c.fail,
							   c.complete =
								   c.whatever =
								   c.always,
							   c.done(d.cancel).fail(c.cancel),
							   delete c.cancel, a
								   && a.call(c,
										   c),
							   c }, when:
								   function(a)
								   { function
									   f(a)
									   {
										   return

	    function(c) { b[a] = arguments.length > 1 ?
		    QQWB.Array.fromArguments(arguments) : c, --d ||
							  e.resolveWith(e,
									  QQWB.Array.fromArguments(b))
	    } } var b = arguments, c = b.length, d = c, e = c <= 1 && a &&
									   QQWB.Function.isFunction(a.promise)
										   ?
										   a
										   :
										   QQWB.deferred.deferred();
									   if
										   (c
										    >
										    1)
											   for
												   (var
												    g
												    =
												    0;
												    g
												    <
												    c;
												    g++)
													   b[g]
													   &&
													   QQWB.Function.isFunction(b[g].promise)
													   ?
													   b[g].promise().then(f(g),
															   e.reject)
													   :
													   --d,
													   d
														   ||
														   e.resolveWith(e,
																   b);
									   else
										   e
											   !==
											   a
											   &&
											   e.resolveWith(e,
													   c
													   ?
													   [a]
													   :
													   []);
									   return
										   e.promise()
								   } }),
								   QQWB._alias(["task",
										   "when"],
										   QQWB.deferred.when),
								   QQWB.extend("io",
										   {
											   _globalIOTimeout:
									   3e4,
								   _IOScript:
									   function(a)
								   { var b, c,
									   d =
									   document.head
									   ||
									   document.getElementsByTagName("head")[0]
									   ||
									   document.documentElement;
								   return {
									   send:
									   function(e)
								   { var f =
									   QQWB.time.now();
									   b =
									   document.createElement("script"),
								   b.async =
									   "async",
								   a.charset &&
									   (b.charset
									    =
									    a.charset),
								   b.src =
									   a.url,
								   c =
									   setTimeout(function()
											   {
												   QQWB.log.warning("script
													   loading
													   timeout"),
												   e(599,
													   "network
													   connect
													   timeout",
													   QQWB.time.now()
													   -
													   f)
											   },
											   QQWB.io._globalIOTimeout),
								   b.onload =
									   b.onreadystatechange
									   =
									   function(a,
											   g)
									   { if
										   (g
										    ||
										    !b.readyState
										    ||
										    /loaded|complete/.test(b.readyState))
											   clearTimeout(c),
										   b.onload
											   =
											   b.onreadystatechange
											   =
											   null,
										   d
											   &&
											   b.parentNode
											   &&
											   d.removeChild(b),
										   b
											   =
											   null,
										   !g
											   &&
											   e
											   &&
											   e.apply(QQWB,
													   [200,
													   "success",
													   QQWB.time.now()
													   -
													   f]),
										   g
											   &&
											   e
											   &&
											   e.apply(QQWB,
													   [-1,
													   "aborted",
													   QQWB.time.now()
													   -
													   f])
									   },
								   b.onerror =
									   function(a)
									   {
										   clearTimeout(c),
										   e
											   &&
											   e.apply(QQWB,
													   [404,
													   a,
													   QQWB.time.now()
													   -
													   f])
									   },
								   d.insertBefore(b,
										   d.firstChild)
								   }, abort:
								   function() {
									   b &&
										   b.onload(0,
												   1)
								   } } },
								   _IOAjax:
									   function(a)
									   {
										   var
											   b,
										   c,
										   a
											   =
											   a
											   ||
											   {},
										   d
											   =
											   window.XMLHttpRequest
											   ?
											   new
											   window.XMLHttpRequest
											   :
											   new
											   window.ActiveXObject("Microsoft.XMLHTTP");
										   return
											   a.async
											   ||
											   (a.async
											    =
											    "async"),
											   {
												   send:
													   function(e)
													   {
														   var
															   f
															   =
															   QQWB.time.now();
														   a.username
															   ?
															   d.open(a.type,
																	   a.url,
																	   a.async,
																	   a.username,
																	   a.password)
															   :
															   d.open(a.type,
																	   a.url,
																	   a.async);
														   try
														   {
															   a.type
																   ==
																   "POST"
																   &&
																   d.setRequestHeader("Content-Type",
																		   "application/x-www-form-urlencoded"),
																   d.setRequestHeader("X-Requested-With",
																		   "XMLHttpRequest"),
																   d.setRequestHeader("X-Requested-From",
																		   "openjs")
														   }
														   catch
															   (g)
															   {}
														   d.send(a.data
																   ||
																   null),
															   c
																   =
																   setTimeout(function()
																		   {
																			   QQWB.log.warning("request
																				   timeout"),
																			   e(599,
																				   "network
																				   connect
																				   timeout",
																				   QQWB.time.now()
																				   -
																				   f)
																		   },
																		   QQWB.io._globalIOTimeout),
															   b
																   =
																   function(g,
																		   h)
																   {
																	   var
																		   i,
																	   j,
																	   k,
																	   l,
																	   m,
																	   n;
																	   try
																	   {
																		   if
																			   (b
																			    &&
																			    (h
																			     ||
																			     d.readyState
																			     ===
																			     4))
																			   {
																				   b
																					   =
																					   null;
																				   if
																					   (h)
																						   d.readyState
																						   !==
																						   4
																						   &&
																						   d.abort();
																				   else
																				   {
																					   i
																						   =
																						   d.status,
																						   k
																							   =
																							   d.getAllResponseHeaders(),
																						   l
																							   =
																							   {},
																						   n
																							   =
																							   d.responseXML,
																						   n
																							   &&
																							   n.documentElement
																							   &&
																							   (l.xml
																							    =
																							    n),
																						   l.text
																							   =
																							   d.responseText;
																					   try
																					   {
																						   j
																							   =
																							   d.statusText
																					   }
																					   catch
																						   (o)
																						   {
																							   j
																								   =
																								   ""
																						   }
																					   i
																						   ===
																						   1223
																						   &&
																						   (i
																						    =
																						    204),
																						   a.dataType.toLowerCase()
																							   ==
																							   "json"
																							   ?
																							   m
																							   =
																							   QQWB.JSON.fromString(l.text)
																							   :
																							   a.dataType.toLowerCase()
																							   ==
																							   "xml"
																							   ?
																							   m
																							   =
																							   l.xml
																							   :
																							   m
																							   =
																							   l.text
																				   }
																				   clearTimeout(c),
																					   e(i,
																							   j,
																							   QQWB.time.now()
																							   -
																							   f,
																							   m,
																							   l.text,
																							   k,
																							   a.dataType)
																			   }
																	   }
																	   catch
																		   (p)
																		   {
																			   var
																				   q
																				   =
																				   p;
																			   i
																				   =
																				   -2,
																				   j
																					   =
																					   "",
																				   j
																					   +=
																					   "Exception
																					   [Type:",
																				   j
																					   +=
																					   q
																					   &&
																					   q.type
																					   ?
																					   q.type
																					   :
																					   "unknown
																					   exception
																					   type",
																				   j
																					   +=
																					   ",
																				   Message:",
																				   j
																					   +=
																					   q
																					   &&
																					   q.message
																					   ?
																					   q.message
																					   :
																					   q,
																				   j
																					   +=
																					   "]",
																				   QQWB.log.warning("caught
																						   "
																						   +
																						   j
																						   +
																						   "
																						   exception
																						   QQWB.io._IOAjax"),
																				   h
																					   ||
																					   (clearTimeout(c),
																					    e(i,
																						    j,
																						    QQWB.time.now()
																						    -
																						    f))
																		   }
																   },
															   !a.async
																   ||
																   d.readyState
																   ===
																   4
																   ?
																   b()
																   :
																   d.onreadystatechange
																   =
																   b
													   },
												   abort:
													   function()
													   {
														   b
															   &&
															   b(0,
																	   1)
													   }
											   }
									   },
								   _IOFlash:
									   function(a)
									   {
										   var
											   b,
										   c,
										   d,
										   a
											   =
											   a
											   ||
											   {};
										   return
										   {
											   send:
												   function(e)
												   {
													   var
														   f
														   =
														   QQWB.time.now();
													   d
														   =
														   1,
														   c
															   =
															   setTimeout(function()
																	   {
																		   QQWB.log.warning("request
																			   timeout"),
																		   e(599,
																			   "network
																			   connect
																			   timeout",
																			   QQWB.time.now()
																			   -
																			   f)
																	   },
																	   QQWB.io._globalIOTimeout),
														   b
															   =
															   function(d,
																	   g)
															   {
																   var
																	   h,
																   i,
																   j,
																   k,
																   l,
																   m,
																   n
																	   =
																	   4;
																   clearTimeout(c);
																   try
																   {
																	   b
																		   &&
																		   (g
																		    ||
																		    n
																		    ==
																		    4)
																		   &&
																		   (b
																		    =
																		    null,
																		    g
																		    ?
																		    e(-1,
																			    "request
																			    has
																			    aborted",
																			    QQWB.time.now()
																			    -
																			    f)
																		    :
																		    (h
																		     =
																		     this.httpStatus,
																		     i
																		     =
																		     this.httpStatus
																		     ==
																		     200
																		     ?
																		     "ok"
																		     :
																		     "",
																		     j
																		     =
																		     "",
																		     k
																		     =
																		     {},
																		     k.text
																		     =
																		     this.httpResponseText,
																		     a.dataType.toLowerCase()
																			     ==
																			     "json"
																			     ?
																			     l
																			     =
																			     QQWB.JSON.fromString(k.text)
																			     :
																			     a.dataType.toLowerCase()
																			     ==
																			     "xml"
																			     ?
																			     l
																			     =
																			     QQWB.XML.fromString(k.text)
																			     :
																			     l
																			     =
																			     k.text),
																		     e(h,
																				     i,
																				     QQWB.time.now()
																				     -
																				     f,
																				     l,
																				     k.text,
																				     j,
																				     a.dataType))
																   }
																   catch
																	   (o)
																	   {
																		   var
																			   p
																			   =
																			   o;
																		   h
																			   =
																			   -2,
																			   i
																				   =
																				   "",
																			   i
																				   +=
																				   "Exception
																				   [Type:",
																			   i
																				   +=
																				   p
																				   &&
																				   p.type
																				   ?
																				   p.type
																				   :
																				   "unknown
																				   exception
																				   type",
																			   i
																				   +=
																				   ",
																			   Message:",
																			   i
																				   +=
																				   p
																				   &&
																				   p.message
																				   ?
																				   p.message
																				   :
																				   p,
																			   i
																				   +=
																				   "]",
																			   QQWB.log.warning("caught
																					   "
																					   +
																					   i
																					   +
																					   "
																					   exception
																					   QQWB.io._IOFlash"),
																			   g
																				   ||
																				   e(h,
																						   i,
																						   QQWB.time.now()
																						   -
																						   f)
																	   }
															   },
														   window.onFlashRequestComplete_8df046
															   ||
															   (window.onFlashRequestComplete_8df046
															    =
															    function(a)
															    {
																    if
																   (!a.ticket)
															   {
																   QQWB.log.error("ticket
																	   doesn't
																	   exists
																	   in
																	   response,
																	   "
																	   +
																	   QQWB.JSON.stringify(a));
																   return
															   }
															   var
																   b
																   =
																   window.onFlashRequestComplete_8df046.callbacks.getByTicket(a.ticket),
																   c
																	   =
																	   a.srcEvent;
															   b.readyState
																   ||
																   (b.readyState
																    =
																    0),
																   /httpStatus/i.test(c.type)
																	   ?
																	   (b.httpStatus
																	    =
																	    c.status,
																	    b.readyState++)
																	   :
																	   /error/i.test(c.type)
																	   ?
																	   (b.httpError
																	    =
																	    c.type,
																	    b.readyState++)
																	   :
																	   /complete/i.test(c.type)
																	   &&
																	   (b.httpResponseText
																	    =
																	    c.target.data,
																	    b.readyState++),
																   b.readyState
																	   ==
																	   2
																	   &&
																	   (b.call(b),
																	    window.onFlashRequestComplete_8df046.callbacks.unregister(a.ticket))
															    },
														   window.onFlashRequestComplete_8df046.callbacks
															   =
															   {
																   _callbackPool:
																   {},
																   _ticketPrefix:
																	   "openjstkt",
																   _ticketStartIndex:
																	   0,
																   register:
																	   function(a)
																	   {
																		   var
																			   b;
																		   return
																			   this._ticketStartIndex++,
																			   b
																				   =
																				   this._ticketPrefix
																				   +
																				   this._ticketStartIndex,
																			   this._callbackPool[b]
																				   =
																				   a,
																			   b
																	   },
																   getByTicket:
																	   function(a)
																	   {
																		   return
																			   this._callbackPool[a]
																			   ||
																			   QQWB.log.error("get
																					   callback
																					   failed,
																					   callback
																					   doesn't
																					   exist
																					   at
																					   ticket
																					   "
																					   +
																					   a),
																		   this._callbackPool[a]
																	   },
																   unregister:
																	   function(a)
																	   {
																		   return
																			   this._callbackPool[a]
																			   ?
																			   (delete
																			    this._callbackPool[a],
																			    !0)
																			   :
																			   (QQWB.log.error("unregister
																					   callback
																					   failed,
																					   callback
																					   doesn't
																					   exist
																					   at
																					   ticket
																					   "
																					   +
																					   a),
																			    !1)
																	   }
															   }),
														   QQWBFlashTransport
															   &&
															   QQWBFlashTransport.httpRequest
															   ?
															   QQWBFlashTransport.httpRequest(a.url,
																	   a.data,
																	   a.type,
																	   window.onFlashRequestComplete_8df046.callbacks.register(b))
															   :
															   QQWB.log.critical("flash
																	   transportation
																	   object
																	   error"
																	   +
																	   QQWBFlashTransportName)
												   },
												   abort:
													   function()
													   {
														   b
															   &&
															   b(0,
																	   1)
													   }
										   }
									   },
								   _apiAjax:
									   function(a,
											   b,
											   c,
											   d)
									   {
										   var
											   e
											   =
											   {
												   type:
													   d.toUpperCase(),
												   url:
													   QQWB._domain.api
													   +
													   a,
												   data:
													   QQWB.queryString.encode(b),
												   dataType:
													   c
											   };
										   return
											   e.type
											   ==
											   "GET"
											   &&
											   (e.url
											    +=
											    e.data
											    ?
											    "?"
											    +
											    e.data
											    :
											    "",
											    delete
											    e.data),
											   QQWB.io.ajax(e)
									   },
								   _apiFlashAjax:
									   function(a,
											   b,
											   c,
											   d)
									   {
										   var
											   e
											   =
											   {
												   type:
													   d.toUpperCase(),
												   url:
													   QQWB._domain.api
													   +
													   a,
												   data:
													   QQWB.queryString.encode(b),
												   dataType:
													   c
											   };
										   return
											   e.type
											   ==
											   "GET"
											   &&
											   (e.url
											    +=
											    e.data
											    ?
											    "?"
											    +
											    e.data
											    :
											    "",
											    delete
											    e.data),
											   QQWB.io.flashAjax(e)
									   },
								   _apiResponder:
									   function(a)
									   {
										   return

	function(b, c, d, e, f, g, h) { var i, j; b !== 200 ? (b = 2e6 +
			Math.abs(b ? b : 0), a.reject(b, c, d, "")) : typeof(i
			= QQWB._apiProvider._apiParseRetCode(f)) == "number" &&
				0 !== i ? (j =
						QQWB._apiProvider._apiParseErrorCode(f),
						b = 1e6 + i * 1e3 + 500 + (j ?
							j : 0), a.reject(b,
								QQWB._apiProvider._apiGetErrorMessage(i,
									j), d,
								f)) :
				a.resolve(b, c, d, e, g, h) } },
								   _ajaxResponder:
									   function(a)
									   {
										   return

	function(b, c, d, e, f, g, h) { b !== 200 ? a.reject(b, c, d, "") :
		a.resolve(e, d, f) } }, flashAjax: function(a) { var b =
			QQWB.deferred.deferred(), c = { type: "get", dataType:
				"json" }; return QQWB.extend(c, a, !0),
			QQWB.io._IOFlash(c).send(QQWB.io._apiResponder(b)),
			b.promise() }, ajax: function(a) { var b =
				QQWB.deferred.deferred(), c = { type: "get",
					dataType: "json" }; return
						QQWB.extend(c, a, !0),
					QQWB.io._IOAjax(c).send(QQWB.io._apiResponder(b)),
					b.promise() }, ajax2: function(a) { var
						b = QQWB.deferred.deferred(), c
							= { type: "get",
								dataType:
									"json"
							}; return
						QQWB.extend(c, a, !0),
							QQWB.io._IOAjax(c).send(QQWB._ajaxResponder(b)),
							b.promise() }, script:
								function(a, b)
								{ var b = b ||
									"utf-8",
									c =
										QQWB.deferred.deferred();
									return
										QQWB.io._IOScript({
											charset:
											b,
											url:
											a
										}).send(function(a,
												b,
												d)
											{
												a
											!==
											200
											?
											c.reject(a,
												b,
												d)
											:
											c.resolve(a,
												b,
												d)
											}),
										c.promise()
								}, jsonp:
						function(a) { var b =
							QQWB.deferred.deferred(),
							c = "callback", d =
								"jsonp_", e = d
								+ QQWB.uid(), f
								=
								window.callbackName,
							g, h = { dataType:
								"text",
								charset:
									"utf-8",
								url: "" };
							return QQWB.extend(h,
									a, !0),
							       h.data && (h.url
									       +=
									       "?"
									       +
									       h.data
									       +
									       "&"
									       +
									       c
									       +
									       "="
									       +
									       e),
							       window[e] =
								       function(a)
								       { var c
									       =
										       a;
									       h.dataType.toLowerCase()
										       ===
										       "json"
										       ?
										       c
										       =
										       QQWB.JSON.fromString(a)
										       :
										       h.dataType.toLowerCase()
										       ===
										       "xml"
										       &&
										       (c
											=
											QQWB.XML.fromString(a)),
										       b.resolve(c,
												       g),
										       window[e]
											       =
											       f
								       },
							       QQWB.io._IOScript(h).send(function(a,
										       c,
										       d)
									       {
										       a
								       !== 200
								       &&
								       b.reject(a,
									       c,
									       d),
							       g = d }),
							       b.promise() }
										   }),
										   QQWB._alias("ajax",
												   QQWB.io.ajax2),
										   QQWB._alias("jsonp",
												   QQWB.io.jsonp),
										   QQWB._alias("script",
												   QQWB.io.script),
										   QQWB.extend("_token",
												   {
													   setAccessToken:
											   function(a,
												   b,
												   c,
												   d)
											   {
												   var
											   e
											   =
											   this.getTokenUser(!0);
										   return
											   QQWB.cookie.set(QQWB._cookie.names.accessToken,
												   [a,
												   QQWB.time.now()
												   +
												   b
												   *
												   1e3,
												   c
												   ||
												   e
												   &&
												   e.name
												   ||
												   "",
												   d
												   ||
												   e
												   &&
												   e.nick
												   ||
												   ""].join("|"),
												   31536e3,
												   QQWB._cookie.path,
												   QQWB._cookie.domain),
												   QQWB
											   },
										   getAccessToken:
											   function(a)
											   {
												   var
													   b
													   =
													   QQWB.cookie.get(QQWB._cookie.names.accessToken);
												   if
													   (b)
													   {
														   b
															   =
															   b.split("|",
																	   2);
														   if
															   (a
															    ||
															    parseInt(b[1],
																    10)
															    >
															    QQWB.time.now())
																   return
																   b[0]
													   }
											   },
										   getTokenUser:
											   function(a)
											   {
												   var
													   b
													   =
													   QQWB.cookie.get(QQWB._cookie.names.accessToken);
												   if
													   (b)
													   {
														   b
															   =
															   b.split("|",
																	   4);
														   if
															   (a
															    ||
															    parseInt(b[1],
																    10)
															    >
															    QQWB.time.now())
																   return
																   {
																	   name:
																		   b[2],
																	   nick:
																		   b[3]
																   }
													   }
											   },
										   clearAccessToken:
											   function()
											   {
												   return
													   QQWB.cookie.del(QQWB._cookie.names.accessToken,
															   QQWB._cookie.path,
															   QQWB._cookie.domain),
												   QQWB
											   },
										   setRefreshToken:
											   function(a)
											   {
												   return
													   QQWB.cookie.set(QQWB._cookie.names.refreshToken,
															   a,
															   31536e3,
															   QQWB._cookie.path,
															   QQWB._cookie.domain),
												   QQWB
											   },
										   getRefreshToken:
											   function()
											   {
												   return
													   QQWB.cookie.get(QQWB._cookie.names.refreshToken)
											   },
										   clearRefreshToken:
											   function()
											   {
												   return
													   QQWB.cookie.del(QQWB._cookie.names.refreshToken,
															   QQWB._cookie.path,
															   QQWB._cookie.domain),
												   QQWB
											   },
										   exchangeForToken:
											   function(a)
											   {
												   return
													   QQWB.io.jsonp({
														   url:
														   QQWB._domain.exchange,
													   data:
														   QQWB.queryString.encode({
															   response_type:
															   "token",
														   client_id:
															   QQWB.appkey.value,
														   scope:
															   "all",
														   state:
															   "1",
														   refresh_token:
															   this.getRefreshToken(),
														   access_token:
															   this.getAccessToken(!0)
														   })
													   }).success(function(a)
														   {
															   var
														   b
														   =
														   a;
													   QQWB.String.isString(a)
														   &&
														   (a
														    =
														    QQWB.queryString.decode(a)),
													   a.access_token
														   ?
														   (!a.expires_in
														    &&
														    QQWB.log.error("token
															    expires_in
															    not
															    retrieved"),
														    !a.wb_name
														    &&
														    QQWB.log.warning("weibo
															    username
															    not
															    retrieved,
															    will
															    not
															    update
															    username"),
														    !a.wb_nick
														    &&
														    QQWB.log.warning("weibo
															    usernick
															    not
															    retrieved,
															    will
															    not
															    update
															    usernick"),
													   QQWB._token.setAccessToken(a.access_token,
															   parseInt(a.expires_in,
																   10),
															   a.wb_name,
															   a.wb_nick),
													   a.refresh_token
														   ?
														   QQWB._token.setRefreshToken(a.refresh_token)
														   :
														   QQWB.log.error("refresh
																   token
																   not
																   retrieved"),
													   QQWB.log.info("exchange
															   token
															   succeed"))
															   :
															   a.error
															   ?
															   QQWB.log.error("exchange
																	   token
																	   error
																	   "
																	   +
																	   a.error)
															   :
															   QQWB.log.error("unexpected
																	   result
																	   returned
																	   from
																	   server
																	   "
																	   +
																	   b
																	   +
																	   "
																	   while
																	   exchanging
																	   for
																	   new
																	   access
																	   token")
														   }).error(function(a,
																   b)
															   {
																   a
															   ===
															   404
															   ?
															   QQWB.log.error("exchange
																   token
																   has
																   failed,
																   script
																   not
																   found")
															   :
															   QQWB.log.error("exchange
																   token
																   has
																   failed,
																   "
																   +
																   b)
															   }).complete(function(b,
																	   c,
																	   d)
																   {
																	   a
																   &&
																   a.apply(QQWB,
																	   [b,
																	   c,
																	   d])
																   }),
													   QQWB
											   },
										   getNewAccessToken:
											   function(a)
											   {
												   return
													   QQWB.io.jsonp({
														   url:
														   QQWB._domain.query,
													   data:
														   QQWB.queryString.encode({
															   response_type:
															   "token",
														   client_id:
															   QQWB.appkey.value,
														   scope:
															   "all",
														   state:
															   "1"
														   })
													   }).success(function(a)
														   {
															   var
														   b
														   =
														   a;
													   QQWB.String.isString(a)
														   &&
														   (a
														    =
														    QQWB.queryString.decode(a)),
													   a.access_token
														   ?
														   (!a.expires_in
														    &&
														    QQWB.log.error("token
															    expires_in
															    not
															    retrieved"),
														    !a.wb_name
														    &&
														    QQWB.log.warning("weibo
															    username
															    not
															    retrieved"),
														    !a.wb_nick
														    &&
														    QQWB.log.warning("weibo
															    usernick
															    not
															    retrieved"),
														    QQWB._token.setAccessToken(a.access_token,
															    parseInt(a.expires_in,
																    10),
															    a.wb_name,
															    a.wb_nick),
														    a.refresh_token
															    ?
															    QQWB._token.setRefreshToken(a.refresh_token)
															    :
															    QQWB.log.error("refresh
																	    token
																	    not
																	    retrieved"),
														    QQWB.log.info("retrieve
																    new
																    access
																    token
																    succeed"))
																    :
																    a.error
																    ?
																    QQWB.log.error("retrieve
																		    new
																		    access
																		    token
																		    error
																		    "
																		    +
																		    a.error)
																    :
																    QQWB.log.error("unexpected
																		    result
																		    returned
																		    from
																		    server
																		    "
																		    +
																		    b
																		    +
																		    "
																		    while
																		    retrieving
																		    new
																		    access
																		    token")
														   }).error(function(a,
																   b)
															   {
																   a
															   ===
															   404
															   ?
															   QQWB.log.error("get
																   token
																   has
																   failed,
																   script
																   not
																   found")
															   :
															   QQWB.log.error("get
																   token
																   failed,
																   "
																   +
																   b)
															   }).complete(function(b,
																	   c,
																	   d)
																   {
																	   a
																   &&
																   a.apply(QQWB,
																	   [b,
																	   c,
																	   d])
																   }),
														   QQWB
											   },
										   resolveResponse:
											   function(a,
													   b)
											   {
												   var
													   c,
												   d
													   =
													   (b
													    ||
													    window).QQWB,
												   e
													   =
													   QQWB.String.isString(a)
													   ?
													   d.queryString.decode(a)
													   :
													   a;
												   e.access_token
													   ?
													   (d._token.setAccessToken(e.access_token,
																    parseInt(e.expires_in,
																	    10),
																    e.wb_name,
																    e.wb_nick),
													    e.refresh_token
													    ?
													    d._token.setRefreshToken(e.refresh_token)
													    :
													    d.log.error("refresh
														    token
														    not
														    retrieved"),
													    c
													    =
													    d.loginStatus(),
													    d.log.info("user
														    "
														    +
														    c.name
														    +
														    "
														    logged
														    in"),
													    d.trigger(d.events.USER_LOGGEDIN_EVENT,
															    c))
																    :
																    e.error
																    ?
																    (d.log.error("login
																		 error
																		 occurred
																		 "
																		 +
																		 e.error),
																     e.message
																     =
																     e.error,
																     d.trigger(d.events.USER_LOGIN_FAILED_EVENT,
																	     e))
																    :
																    (d.log.error("unexpected
																		 result
																		 returned
																		 from
																		 server
																		 "
																		 +
																		 a),
																     e.message
																     =
																     e.error
																     =
																     "server
																     error",
																     d.trigger(d.events.USER_LOGIN_FAILED_EVENT,
																	     e))
											   }
												   }),
												   QQWB.extend("_eventProvider",
														   {
															   _getEventsMap:
													   function()
												   {
													   return
													   this._eventsMap
													   ||
													   (this._eventsMap
													    =
													    {}),
												   this._eventsMap
												   },
												   bind:
													   function(a,
														   b)
													   {
														   var
													   c
													   =
													   this._getEventsMap();
														   c[a]
															   ?
															   QQWB.Array.inArray(c[a],
																	   b)
															   ||
															   c[a].push(b)
															   :
															   c[a]
															   =
															   [b]
													   },
												   unbind:
													   function(a,
															   b)
													   {
														   var
															   c
															   =
															   this._getEventsMap()[a];
														   if
															   (c)
																   if
																	   (b)
																		   for
																			   (var
																			    d
																			    =
																			    0,
																			    e
																			    =
																			    c.length;
																			    d
																			    <
																			    e;
																			    d++)
																				   b
																				   ===
																				   c[d]
																				   &&
																				   (c[d]
																				    =
																				    null);
																   else
																	   delete
																		   this._getEventsMap()[a]
													   },
												   trigger:
													   function(a,
															   b)
													   {
														   var
															   c
															   =
															   this._getEventsMap()[a];
														   if
															   (c)
																   for
																	   (var
																	    d
																	    =
																	    0,
																	    e
																	    =
																	    c.length;
																	    d
																	    <
																	    e;
																	    d++)
																	   {
																		   var
																			   f
																			   =
																			   c[d];
																		   f
																			   &&
																			   f.call(QQWB,
																					   b)
																	   }
													   }
														   }),
														   QQWB.extend("",
																   {
																	   bind:
															   function(a,
																   b)
															   {
																   return
															   a
															   =
															   a.toLowerCase(),
														   this._eventProvider.bind(a,
															   b),
														   this
															   },
														   once:
															   function(a,
																   b)
															   {
																   a
															   =
															   a.toLowerCase();
																   var
																	   c
																	   =
																	   function()
																	   {
																		   var
																			   d
																			   =
																			   QQWB.Array.fromArguments(arguments);
																		   b.apply(QQWB,
																				   d),
																			   this._eventProvider.unbind(a,
																					   c),
																			   c
																				   =
																				   null
																	   };
																   return
																	   this._eventProvider.bind(a,
																			   c),
																	   this
															   },
														   unbind:
															   function(a,
																	   b)
															   {
																   return
																	   a
																	   =
																	   a.toLowerCase(),
																   this._eventProvider.unbind(a,
																		   b),
																   this
															   },
														   trigger:
															   function(a,
																	   b)
															   {
																   return
																	   a
																	   =
																	   a.toLowerCase(),
																   this._eventProvider.trigger(a,
																		   b),
																   this
															   }
																   }),
																   QQWB.extend("events",
																		   {
																			   USER_LOGGEDIN_EVENT:
																	   "UserLoggedIn",
																   USER_LOGIN_FAILED_EVENT:
																	   "UserLoginFailed",
																   USER_LOGGEDOUT_EVENT:
																	   "UserLoggedOut",
																   TOKEN_READY_EVENT:
																	   "tokenReady",
																   DOCUMENT_READY_EVENT:
																	   "documentReady",
																   EVERYTHING_READY_EVENT:
																	   "everythingReady"
																		   }),
																   function()
{ function j(a) { a = a.toLowerCase(); var b = e.exec(a) || d.exec(a) ||
	c.exec(a) || a.indexOf("compatible") < 0 && f.exec(a) || []; return {
		browser: b[1] || "unknown", version: b[2] || "0" } }

    function k() { for (var a in i) i.hasOwnProperty(a) && i[a]() && (g[a] =
		    !0) }

    function l() { var a = navigator.userAgent || navigator.vendor ||
	    window.opera; return
		    /android.+mobile|avantgo|bada\/|blackberry|blazer|compal|elaine|fennec|hiptop|iemobile|ip(hone|od)|iris|kindle|lge
		    |maemo|midp|mmp|opera m(ob|in)i|palm(
				    os)?|phone|p(ixi|re)\/|plucker|pocket|psp|symbian|treo|up\.(browser|link)|vodafone|wap|windows
		    (ce|phone)|xda|xiino/i.test(a) ||
		    /1207|6310|6590|3gso|4thp|50[1-6]i|770s|802s|a
		    wa|abac|ac(er|oo|s\-)|ai(ko|rn)|al(av|ca|co)|amoi|an(ex|ny|yw)|aptu|ar(ch|go)|as(te|us)|attw|au(di|\-m|r
				    |s)|avan|be(ck|ll|nq)|bi(lb|rd)|bl(ac|az)|br(e|v)w|bumb|bw\-(n|u)|c55\/|capi|ccwa|cdm\-|cell|chtm|cldc|cmd\-|co(mp|nd)|craw|da(it|ll|ng)|dbte|dc\-s|devi|dica|dmob|do(c|p)o|ds(12|\-d)|el(49|ai)|em(l2|ul)|er(ic|k0)|esl8|ez([4-7]0|os|wa|ze)|fetc|fly(\-|_)|g1
		    u|g560|gene|gf\-5|g\-mo|go(\.w|od)|gr(ad|un)|haie|hcit|hd\-(m|p|t)|hei\-|hi(pt|ta)|hp(
				    i|ip)|hs\-c|ht(c(\-|
						    |_|a|g|p|s|t)|tp)|hu(aw|tc)|i\-(20|go|ma)|i230|iac(
						    |\-|\/)|ibro|idea|ig01|ikom|im1k|inno|ipaq|iris|ja(t|v)a|jbro|jemu|jigs|kddi|keji|kgt(
							    |\/)|klon|kpt
						    |kwc\-|kyo(c|k)|le(no|xi)|lg(
								    g|\/(k|l|u)|50|54|e\-|e\/|\-[a-w])|libw|lynx|m1\-w|m3ga|m50\/|ma(te|ui|xo)|mc(01|21|ca)|m\-cr|me(di|rc|ri)|mi(o8|oa|ts)|mmef|mo(01|02|bi|de|do|t(\-|
									    |o|v)|zz)|mt(50|p1|v)|mwbp|mywa|n10[0-2]|n20[2-3]|n30(0|2)|n50(0|2|5)|n7(0(0|1)|10)|ne((c|m)\-|on|tf|wf|wg|wt)|nok(6|i)|nzph|o2im|op(ti|wv)|oran|owg1|p800|pan(a|d|t)|pdxg|pg(13|\-([1-8]|c))|phil|pire|pl(ay|uc)|pn\-2|po(ck|rt|se)|prox|psio|pt\-g|qa\-a|qc(07|12|21|32|60|\-[2-7]|i\-)|qtek|r380|r600|raks|rim9|ro(ve|zo)|s55\/|sa(ge|ma|mm|ms|ny|va)|sc(01|h\-|oo|p\-)|sdk\/|se(c(\-|0|1)|47|mc|nd|ri)|sgh\-|shar|sie(\-|m)|sk\-0|sl(45|id)|sm(al|ar|b3|it|t5)|so(ft|ny)|sp(01|h\-|v\-|v)|sy(01|mb)|t2(18|50)|t6(00|10|18)|ta(gt|lk)|tcl\-|tdg\-|tel(i|m)|tim\-|t\-mo|to(pl|sh)|ts(70|m\-|m3|m5)|tx\-9|up(\.b|g1|si)|utst|v400|v750|veri|vi(rg|te)|vk(40|5[0-3]|\-v)|vm40|voda|vulc|vx(52|53|60|61|70|80|81|83|85|98)|w3c(\-|)|webc|whit|wi(g
									    |nc|nw)|wmlb|wonu|x700|xda(\-|2|g)|yas\-|your|zeto|zte\-/i.test(a.substr(0,
											    4))
									    ? {
										    mobile:
											    !0
									    } :
	    { pc: !0 } }

    function m() { var a = navigator.appVersion, b = {}, c = "unknown"; return
	    a.indexOf("Win") != -1 && (c = "windows"), a.indexOf("Mac") != -1
		    && (c = "mac"), a.indexOf("X11") != -1 && (c = "unix"),
	    a.indexOf("Linux") != -1 && (c = "linux"), b[c] = !0, b } var a, b
		    = navigator.userAgent, c = /(msie) ([\w.]+)/, d =
		    /(opera)(?:.*version)?[ \/]([\w.]+)/, e = /(webkit)[
		    \/]([\w.]+)/, f = /(mozilla)(?:.*?  rv:([\w.]+))?/, g = {},
	    h = ["Webkit", "Moz", "O", "ms", "khtml"], i = { cookie: function()
		    { var a = navigator.cookieEnabled; if (a &&
				    QQWB.browser.webkit) { var b =
					    "COOKIE_TEST_" + QQWB.uid();
					    document.cookie = b + "=" + 1 + ";
					    domain=; path=;",
						    document.cookie.indexOf(b)
							    < 0 ? a = !1 :
							    document.cookie = b
							    + "=" + ";
					    expires=" + (new Date(1970, 1,
						    1)).toUTCString() + ";
					    domain=; path=;" } return !a &&
						    QQWB.log.critical("Your
								    browser
								    doesn't
								    support
								    cookie or
								    cookie
								    isn't
								    enabled"),
						    a }, flash: function() { if
							    (typeof
							     navigator.plugins
							     != "undefined" &&
							     typeof
							     navigator.plugins["Shockwave
							     Flash"] ==
							     "object") { var a
								     =
									     navigator.plugins["Shockwave
									     Flash"].description,
								     b = typeof
									     navigator.mimeTypes
									     !=
									     "undefined"
									     &&
									     navigator.mimeTypes["application/x-shockwave-flash"]
									     &&
									     navigator.mimeTypes["application/x-shockwave-flash"].enabledPlugin;
								     return a
									     &&
									     b
							     } if (typeof
									     window.ActiveXObject
									     !=
									     "undefined")
							     try { var c = new
								     ActiveXObject("ShockwaveFlash.ShockwaveFlash");
								     if (c)
									     return
										     c.getVariable("$version")
							     } catch (d) {} },
						    userdata: function() {
							    return
								    QQWB.browser.msie
						    }, postmessage: function()
					    { return !!window.postMessage &&
						    (QQWB.browser.msie &&
						     parseInt(QQWB.browser.version,
							     10) < 8 ? !1 : !0)
					    }, canvas: function() { var a =
						    document.createElement("canvas");
						    return !!a.getContext && !!
							    a.getContext("2d")
					    }, webgl: function() { return
						    !!window.WebGLRenderingContext
					    }, geolocation: function() { return
						    !!navigator.geolocation },
						    websqldatabase: function()
						    { return
							    !!window.openDatabase
						    }, indexeddb: function() {
							    for (var a = 0, b =
									    h.length;
									    a <
									    b;
									    a++)
								    if
									    (window[h[a].toLowerCase()
									     +
									     "IndexedDB"])
										    return
										    !0;
							    return
								    !!window.indexedDB
						    }, websocket: function() {
							    for (var a = 0, b =
									    h.length;
									    a <
									    b;
									    a++)
								    if
									    (window[h[a].toLowerCase()
									     +
									     "WebSocket"])
										    return
										    !0;
							    return
								    !!window.WebSocket
						    }, localstorage: function()
						    { return
							    window.localStorage
								    &&
								    localStorage.getItem
						    }, sessionstorage:
						    function() { return
							    window.sessionStorage
								    &&
								    sessionStorage.getItem
						    }, webworker: function() {
							    return
								    !!window.Worker
						    }, applicationcache:
						    function() { return
							    !!window.applicationCache
						    } }; a = j(b),
	    QQWB.extend("browser", { version: a.version }),
	    QQWB.browser[a.browser] = !0, k(), QQWB.extend("browser.feature",
			    g), QQWB.extend("browser.platform", l()),
	    QQWB.extend("browser.os", m()) }(), QQWB.extend("flash", {
		    NO_CACHE: 1, load: function(a, b, c) { this.loadedSwfs ||
			    (this.loadedSwfs = []); if
		    (QQWB.Array.inArray(this.loadedSwfs, a)) {
			    QQWB.log.warning(a + "is already loaded"); return }
	    c === this.NO_CACHE && (a += "?" + QQWB.uid()); var d =
		    "movieContainer_" + QQWB.uid(), e = "movie_" + QQWB.uid(),
	    f = "onFlashReady_a1f5b4ce", g = window[f]; return window[f] =
		    function() { b && b(e), window[f] = g, g = null, b && (b =
			    null), e = null },
	    QQWB.dom.appendHidden(["<object",
		    'type="application/x-shockwave-flash"', 'id="' + e + '"',
		    QQWB.browser.msie ? 'name="' + e + '"' : "",
		    QQWB.browser.msie ? 'data="' + a + '"' : "",
		    QQWB.browser.msie ?
		    'classid="clsid:d27cdb6e-ae6d-11cf-96b8-444553540000"' :
		    "", 'allowscriptaccess="always">', '<param name="movie"
		    value="' + a + '"></param>', '<param
		    name="allowscriptaccess" value="always"></param>',
		    "</object>"].join(" "), { id: d }, !0),
	    document.getElementById(d) }, getSWFObjectByName: function(a) {
		    return QQWB.browser.msie ? window[a] : document[a].length ?
			    document[a][1] : document[a] } }),
			    QQWB.extend("_solution", { HTML5_SOLUTION: "html5",
				    FLASH_SOLUTION: "flash",
			    SILVER_LIGHT_SOLUTION: "silverlight", initSolution:
				    function(a) { var b, c =
					    QQWB.deferred.deferred(); !this[a]
				    && QQWB.Array.inArray([this.HTML5_SOLUTION,
					    this.FLASH_SOLUTION,
					    this.SILVER_LIGHT_SOLUTION], a) &&
				    (this[a] = {}, this[a].name = a,
				     this[a].readyState = 0, this[a].id =
				     "solution_" + QQWB.uid(), this[a].deferred
				     = QQWB.deferred.deferred(),
				     this[a].promise =
				     this[a].deferred.promise()); if (this[a]
				     && this[a].readyState !== 0)
				     this[a].deferred.success(function() {
					     c.resolve(QQWB.Array.fromArguments(arguments))
				     }).fail(function() {
					     c.reject(QQWB.Array.fromArguments(arguments))
				     }); else switch (a) { case
					     this.HTML5_SOLUTION: if
						     (QQWB.browser.feature.postmessage)
						     { b =
							     this[this.HTML5_SOLUTION];
							     var d =
								     function(a)
								     {
									     QQWB._domain.serverproxy.indexOf(a.origin)
										     !==
										     0
										     ?
										     QQWB.log.warning("unexpected
												     message
												     arrived
												     from
												     "
												     +
												     a.origin
												     +
												     "
												     with
												     data
												     "
												     +
												     a.data)
										     :
										     (a.data
										      ===
										      "success"
										      ?
										      (QQWB.log.info("html5
												     solution
												     was
												     successfully
												     initialized"),
										       b.readyState
										       =
										       1,
										       b.deferred.resolve())
										      :
										      QQWB.log.info("unexpected
											      solution
											      signal
											      "
											      +
											      a.data),
										      window.addEventListener
											      ?
											      window.removeEventListener("message",
													      d,
													      !1)
											      :
											      window.attachEvent
											      &&
											      window.detachEvent("onmessage",
													      d),
									     d
										     =
										     null)
								     };
							     window.addEventListener
								     ?
								     window.addEventListener("message",
										     d,
										     !1)
								     :
								     window.attachEvent
								     &&
								     window.attachEvent("onmessage",
										     d),
								     QQWB.everythingReady(function()
										     {
											     QQWB.log.info("init
												     html5
												     solution..."),
											     serverframe
									     =
									     QQWB.dom.createHidden("iframe",
										     {
											     id:
										     b.id,
									     src:
										     QQWB._domain.serverproxy
										     }),
								     QQWB.dom.append(serverframe),
								     serverframe.onload
									     =
									     function(a)
								     {
									     setTimeout(function()
										     {
											     b.readyState
										     !==
										     1
										     &&
										     (QQWB.log.error("html5
												     solution
												     initialition
												     has
												     failed,
												     server
												     proxy
												     frame
												     encountered
												     error"),
										      b.readyState
										      =
										      2,
										      b.deferred.reject(-1,
											      "server
											      proxy
											      frame
											      not
											      working"))
										     },
								     1e3) } })
						     } else
							     QQWB.log.error("can't
									     init
									     solution
									     \""
									     +
									     a)
								     +
								     "\",browser
								     doesn't
								     support
								     postmessage",
								     c.reject("browser
										     not
										     supported");
						     break; case
							     this.FLASH_SOLUTION:
							     QQWB.browser.feature.flash
							     ? (b =
									     this[this.FLASH_SOLUTION],
									     QQWB.everythingReady(function()
										     {
											     QQWB.log.info("init
												     flash
												     solution...");
											     var
										     a,
									     c
										     =
										     1e4,
									     d
										     =
										     QQWB.flash.load(QQWB._domain.flashproxy,
											     function(c)
											     {
												     QQWB.log.info("flash
													     solution
													     initlized
													     successfully"),
												     b.readyState
											     =
											     1,
										     window.QQWBFlashTransport
											     =
											     QQWB.flash.getSWFObjectByName(c),
										     a
											     &&
											     clearTimeout(a),
										     b.deferred.resolve()
											     },
											     QQWB.flash.NO_CACHE);
									     a
										     =
										     setTimeout(function()
												     {
													     b.deferred.isResolved()
											     ||
											     (b.readyState
											      =
											      2,
											      b.deferred.reject(-1,
												      "encounter
												      error
												      while
												      loading
												      proxy
												      swf,
												      need
												      newer
												      flash
												      player"),
											      QQWB.dom.remove(d))
												     },
												     c)
										     }))
						     : (QQWB.log.error("can't
									     init
									     solution
									     \""
									     +
									     a)
								     +
								     "\",browser
								     doesn't
								     support
								     flash or
								     flash is
								     disabled",
								     c.reject("browser
									     not
									     supported"));
						     break; case
							     this.SILVER_LIGHT_SOLUTION:
							     QQWB.browser.feature.silverlight
							     ? (-2,
									     QQWB.log.error("sorry,
										     silverlight
										     solution
										     is
										     not
										     implemented"))
							     :
							     (QQWB.log.error("can't
									     init
									     solution
									     \""
									     +
									     a)
							      + "\",browser
							      doesn't support
							      silverlight or
							      silverlight is
							      disabled",
							      c.reject("browser
								      not
								      supported"));
						     break; default:
							     QQWB.log.error("can't
									     init
									     solution
									     \""
									     +
									     a)
							     + '",not
							     supported',
							     c.reject("solution
									     "
									     +
									     a
									     +
									     "
									     not
									     supported")
				     } return c.promise() } }),
				     QQWB.extend("ping", { _pingbackURL:
					     "http://btrace.qq.com/collect",
				     _stupidPingParamsOrder: ["ftime", "sIp",
				     "iQQ", "sBiz", "sOp", "iSta", "iTy",
				     "iFlow"], _paramSeprator: ";",
				     _getBasePingParams: function() { var a =
					     QQWB.cookie.get("uin", null,
						     "0").match(/\d+/)[0], b =
						     ""; return { sIp: "", iQQ:
							     a, sBiz:
					     "open-js", sOp: "", iSta: "", iTy:
					     1183, iFlow: b, iFrom: "",
				     iPubFrom: "", sUrl: "", iUrlType: "",
				     iPos: "", sText: "", iBak1: "", iBak2: "",
				     sBak1: "", sBak2: QQWB.uid() } },
				     pingWith: function(a, b) { a =
					     QQWB.extend(QQWB.ping._getBasePingParams(),
						     a, !0),
					     QQWBPingTransport_18035d19 = new
					     Image(1, 1),
				     QQWBPingTransport_18035d19.src =
					     QQWB.ping._pingbackURL + "?" +
					     QQWB.queryString.encode(a, null,
							     null, b) },
				     pingInit: function() { function a() { var
					     a = 1e6; return feature = 0,
					     QQWB.browser.msie ? a += 100 :
						     QQWB.browser.opera ? a +=
						     200 : QQWB.browser.webkit
						     ? a += 300 :
						     QQWB.browser.mozilla ? a
						     += 400 : a += 500,
					     QQWB.browser.feature.postmessage
						     && (feature += 1),
					     QQWB.browser.feature.flash &&
						     (feature += 2),
					     QQWB.browser.feature.cookie &&
						     (feature += 4), a +=
						     feature, a }

	function b() { var a = 1e6; return QQWB.browser.platform.mobile ? a +=
		100 : a += 200, QQWB.browser.os.windows ? a += 10 :
			QQWB.browser.os.windows ? a += 20 : QQWB.browser.os.mac
			? a += 30 : QQWB.browser.os.unix ? a += 40 :
			QQWB.browser.os.linux ? a += 50 : a += 60, a +=
			parseInt(QQWB.appkey.version, 10), a } return
			QQWB.ping.pingWith({ sOp: "init", iFrom:
				QQWB.version.replace(/\./g, ""), iPubFrom: b(),
			sUrl: [document.title,
			document.location.href].join(QQWB.ping._paramSeprator),
			sText: QQWB.appkey.value, iBak1: a() },
			QQWB.ping._stupidPingParamsOrder.concat("iFrom",
				"iPubFrom", "sUrl", "iUrlType", "iPos",
				"sText", "iBak1", "iBak2", "sBak1", "sBak2"))
				     }, _pingAuthorize: function(a) { return
					     QQWB.ping.pingWith({ sOp: "login",
						     iSta: a ? 1 : 0, iFrom:
						     QQWB.version.replace(/\./g,
							     ""), sUrl:
						     document.location.href,
					     sText: QQWB.appkey.value },
					     QQWB.ping._stupidPingParamsOrder.concat("iFrom",
						     "iPubFrom", "sUrl",
						     "iUrlType", "iPos",
						     "sText", "iBak1", "iBak2",
						     "sBak1", "sBak2")) },
						     pingLoggedIn: function() {
							     return
								     QQWB.ping._pingAuthorize(!0)
						     }, pingLoggedInFailed:
					     function() { return
						     QQWB.ping._pingAuthorize(!1)
					     }, pingAPI: function(a, b, c, d,
							     e, f, g, h) { var
								     i = 1e6; a
									     =
									     a
									     ||
									     "",
								     b = b ||
									     "",
								     c = c ||
									     "",
								     d = d ||
									     "",
								     e = e ||
									     "-2",
								     f = f ||
									     "",
								     g = g ||
									     "-1",
								     h = h ||
									     "";
								     switch (h)
								     { case
									     QQWB._solution.HTML5_SOLUTION:
									     case
									     "postmessage":
									     i
									     +=
									     100;
									     break;
									     case
										     QQWB._solution.FLASH_SOLUTION:
										     i
										     +=
										     200;
									     break;
									     case
										     QQWB._solution.SILVER_LIGHT_SOLUTION:
										     i
										     +=
										     400
								     } d =
								     d.toUpperCase();
								     switch (d)
								     { case
									     "GET":
										     i
										     +=
										     10;
									     break;
									     case
										     "POST":
										     i
										     +=
										     20
								     } return
								     QQWB.ping.pingWith({
									     sOp:
									     "api",
									     iSta:
									     e,
									     iFrom:
									     QQWB.version.replace(/\./g,
										     ""),
									     iPubFrom:
									     i,
									     sUrl:
									     document.location.href,
									     sText:
									     QQWB.appkey.value,
									     iBak1:
									     g,
									     sBak1:
									     [a,
									     b].join(QQWB.ping._paramSeprator)
								     },
								     QQWB.ping._stupidPingParamsOrder.concat("iFrom",
									     "iPubFrom",
									     "sUrl",
									     "iUrlType",
									     "iPos",
									     "sText",
									     "iBak1",
									     "iBak2",
									     "sBak1",
									     "sBak2"))
							     } }),
							     QQWB.extend("door",
									     {
										     doors:
								     0, door:
								     function(a,
									     b)
								     { var c =
									     0;
									     return
								     this.doors++,
							     { lock: function()
								     { return
									     c++,
							     a && a.call(QQWB),
							     this }, unlock:
								     function()
							     { return c--, c =
								     Math.max(0,
									     c),
								     b &&
								     b.call(QQWB),
								     this },
							     isOpen: function()
							     { return c === 0 }
							     } }, count:
							     function() {
								     return
									     this.doors
							     } }),
							     QQWB.extend("", {
								     init:
								     function(a)
							     { if (this._inited
								     === !0)
								     return
								     this.log.warning("already
									     initialized"),
							     this;
							     this.log.info("init
								     signal has
								     arrived"),
								     a =
								     QQWB.extend({
									     callbackurl:
									     document.location.href.replace(location.search,
										     "").replace(location.hash,
											     ""),
										     pingback:
									     !0,
								     synclogin:
									     !0
								     }, a, !0),
							     QQWB.pingback =
								     a.pingback;
							     var b =
								     this._token.getAccessToken(),
								     c =
									     this._token.getAccessToken(!0),
								     d =
									     this._token.getRefreshToken(),
								     e = d &&
									     !b
									     &&
									     c,
								     f = !d &&
									     !b
									     &&
									     a.synclogin;
							     return a.appkey &&
								     (this.log.info("client
										    id
										    is
										    "
										    +
										    a.appkey),
								      this.assign("appkey.value",
									      "APPKEY",
									      a.appkey)),
								     this.log.info("client
										     proxy
										     uri
										     is
										     "
										     +
										     a.callbackurl),
								     this.assign("_domain",
										     "CLIENTPROXY_URI",
										     a.callbackurl),
								     (e || f)
									     &&
									     QQWB._tokenReadyDoor.lock(),
								     e ?
									     (this.log.info("exchanging
											    refresh
											    token
											    to
											    access
											    token..."),
									      QQWB._token.exchangeForToken(function(b)
										      {
											      a.synclogin
										      &&
										      b.error
										      &&
										      (QQWB.log.warning("exchange
													token
													has
													failed,
													trying
													to
													retrieve
													a
													new
													access_token..."),
										       this._tokenReadyDoor.lock(),
										       QQWB._token.getNewAccessToken(function()
											       {
												       this._tokenReadyDoor.unlock()
											       })),
									      this._tokenReadyDoor.unlock()
										      }))
							     : f &&
								     (this.log.info("retrieving
										    new
										    access
										    token..."),
								      QQWB._token.getNewAccessToken(function()
									      {
										      QQWB._tokenReadyDoor.unlock()
									      })),
								     /^[a-z\d][a-z\d]{30}[a-z\d]$/i.test(QQWB.appkey.value)
									     ?
									     this.assign("appkey",
											     "APPKEY_VERSION",
											     1)
									     :
									     /^[1-9][0-9]{7}[0-9]$/.test(QQWB.appkey.value)
									     ?
									     this.assign("appkey",
											     "APPKEY_VERSION",
											     2)
									     :
									     this.assign("appkey",
											     "APPKEY_VERSION",
											     3),
								     this._inited
									     =
									     !0,
								     QQWB._tokenReadyDoor.unlock(),
								     this.pingback
									     &&
									     this.ping
									     &&
									     this.ping.pingInit(),
								     this.pingback
									     &&
									     this.ping
									     &&
									     QQWB.bind(QQWB.events.USER_LOGGEDIN_EVENT,
											     this.ping.pingLoggedIn),
								     this.pingback
									     &&
									     this.ping
									     &&
									     QQWB.bind(QQWB.events.USER_LOGIN_FAILED_EVENT,
											     this.ping.pingLoggedInFailed),
								     this },
							     _tokenReadyDoor:
								     QQWB.door.door(function()
										     {
											     this.log.info("tokenReady
												     is
												     locked")
										     },
										     function()
										     {
											     this.log.info("tokenReady
												     is
												     unlocked"),
											     this._tokenReadyDoor.isOpen()
									     &&
									     this.log.info("token
										     is
										     ready")
									     &&
									     this.trigger(this.events.TOKEN_READY_EVENT)
										     }),
							     tokenReady:
								     function(a)
								     { return
									     this._tokenReadyDoor.isOpen()
										     ?
										     a
										     &&
										     a()
										     :
										     this.bind(this.events.TOKEN_READY_EVENT,
												     a),
									     this
								     },
							     _isDocumentReady:
								     !1,
							     _tryToTriggerDocumentReadyEvents:
								     function()
								     { if
									     (this._isDocumentReady)
										     return;
									     try
									     {
										     var
											     a
											     =
											     document.getElementsByTagName("body")[0].appendChild(document.createElement("span"));
										     a.parentNode.removeChild(a)
									     }
									     catch
										     (b)
										     {
											     return
										     }
									     this._isDocumentReady
										     =
										     !0,
										     this.log.info("document
												     is
												     ready"),
										     this._everythingReadyDoor.unlock(),
										     this.trigger(this.events.DOCUMENT_READY_EVENT)
								     },
							     documentReady:
								     function(a)
								     { return
									     this._isDocumentReady
										     ?
										     a
										     &&
										     a()
										     :
										     (this.bind(this.events.DOCUMENT_READY_EVENT,
												a),
										      this._tryToTriggerDocumentReadyEvents()),
									     this
								     },
							     _everythingReadyDoor:
								     QQWB.door.door(function()
										     {
											     this.log.info("everythingReady
												     is
												     locked")
										     },
										     function()
										     {
											     this.log.info("everythingReady
												     is
												     unlocked"),
											     this._everythingReadyDoor.isOpen()
									     &&
									     this.log.info("everything
										     is
										     ready")
									     &&
									     this.trigger(this.events.EVERYTHING_READY_EVENT)
										     }),
							     everythingReady:
								     function(a)
								     { return
									     this._everythingReadyDoor.isOpen()
										     ?
										     a
										     &&
										     a()
										     :
										     this.bind(this.events.EVERYTHING_READY_EVENT,
												     a),
									     this
								     } }),
								     T.alias("ready",
										     "everythingReady"),
								     function()
{ function c() { QQWB.browser.feature.postmessage ?
	QQWB._solution.initSolution(QQWB._solution.HTML5_SOLUTION) :
		QQWB.browser.feature.flash ?
			QQWB._solution.initSolution(QQWB._solution.FLASH_SOLUTION)
			: QQWB.log.error("init solution is called, but no
					solution for the browser") } var a =
			window != window.parent, b = QQWB._domain.serverproxy
			=== window.location.href; QQWB._tokenReadyDoor.lock(),
		QQWB._everythingReadyDoor.lock(),
		QQWB._everythingReadyDoor.lock(),
		QQWB.bind(QQWB.events.TOKEN_READY_EVENT, function() {
			QQWB._everythingReadyDoor.unlock() }); if (a && b &&
			QQWB.browser.feature.postmessage) {
				QQWB.log.info("library booting at server proxy
						mode"); var d = "*", e =
					window.parent; e.postMessage("success",
							d); var f = function(a)
					{ var b = QQWB.JSON.fromString(a.data),
						c = b.id, f = b.data, g = f[0];
						f[2].toLowerCase() == "xml" &&
							(f[2] = "xmltext"), g ?
							QQWB.io._apiAjax.apply(this,
									f).complete(function()
										{
											e.postMessage(QQWB.JSON.stringify({
												id:
												c,
											data:
												QQWB.Array.fromArguments(arguments)
											}),
												d)
										})
						:
							(e.postMessage(QQWB.JSON.stringify({
								id: c, data:
								[-1, "interface
								can not be
								empty"] }), d),
							 QQWB.log.error("interface
								 is empty")) };
						window.addEventListener ?
							window.addEventListener("message",
									f, !1)
							: window.attachEvent &&
							window.attachEvent("onmessage",
									f);
						return } QQWB.log.info("library
								booting at
								normal mode"),
						       c() }(),
						       QQWB._isDocumentReady ||
						       (window.addEventListener
							&&
							document.addEventListener("DOMContentLoaded",
								function() {
									QQWB._tryToTriggerDocumentReadyEvents()
								}, !1),
							window.attachEvent &&
							(document.attachEvent("onreadystatechange",
									      function()
									      {
										      /complete/.test(document.readyState)
								&&
								(document.detachEvent("onreadystatechange",
										      arguments.callee),
								 QQWB._tryToTriggerDocumentReadyEvents())
									      }),
							 window === window.top
							 && function() { if
								 (QQWB._isDocumentReady)
								return; try {
									document.documentElement.doScroll("left")
								} catch (a) {
									setTimeout(arguments.callee,
										0);
									return
								}
							QQWB._tryToTriggerDocumentReadyEvents()
							 }()),
							QQWB.browser.webkit &&
							function() { if
								(QQWB._isDocumentReady)
									return;
								if
									(!/load|complete/.test(document.readyState))
									{
										setTimeout(arguments.callee,
												0);
										return
									}
								QQWB._tryToTriggerDocumentReadyEvents()
							}()), function() {
								function b() {
									var c =
										!!
										QQWB._token.getAccessToken(),
									d; a &&
										QQWB.log.info("cancel
												the
												**OLD**
												maintain
												token
												schedule"),
									a &&
										clearTimeout(a),
									c ? (d
											=
											parseInt(QQWB.cookie.get(QQWB._cookie.names.accessToken).split("|")[1],
												10)
											-
											QQWB.time.now()
											-
											15e3,
											QQWB.log.info("scheduled
												to
												exchange
												token
												after
												"
												+
												d
												+
												"ms"),
											a
											=
											setTimeout(function()
												{
													QQWB._token.exchangeForToken(function()
														{
															b()
														})
												},
												d))
													:
														(a
														 &&
														 QQWB.log.info("cancel
															 the
															 exchange
															 token
															 schedule"),
														 a
														 &&
														 clearTimeout(a))
								} var a;
								QQWB.bind(QQWB.events.TOKEN_READY_EVENT,
										b),
									QQWB.bind(QQWB.events.USER_LOGGEDIN_EVENT,
											b),
									QQWB.bind(QQWB.events.USER_LOGIN_FAILED_EVENT,
											b),
									QQWB.bind(QQWB.events.USER_LOGGEDOUT_EVENT,
											b)
							}(); if
(QQWB.browser.feature.localstorage) QQWB.extend("localStorage", { set:
	function(a, b, c) { a = "k" + a; var d = QQWB.time.secondsNow() + (c ||
		7) * 24 * 3600, e = { value: b, expire: d }; return
		localStorage[a] = JSON.stringify(e), localStorage[a] }, get:
	function(a, b) { a = "k" + a; var c = localStorage[a]; return c && (c =
		JSON.parse(c)) && c.value && QQWB.time.secondsNow() < c.expire
		? c.value : b }, del: function(a) { return a = "k" + a,
			localStorage.removeItem(a), !localStorage[a] } }); else
if (QQWB.browser.feature.userdata) { var userData, storeName =
	"QQWBLocalStore"; QQWB.documentReady(function() { userData =
		document.createElement("input"), userData.type = "hidden",
		userData.style.display = "none",
		userData.addBehavior("#default#userData"), userData.expires =
		(new Date(QQWB.time.now() + 31536e7)).toUTCString(),
		document.body.appendChild(userData) }),
		QQWB.extend("localStorage", { set: function(a, b, c) { a = "k"
			+ a; var d = QQWB.time.secondsNow() + (c || 7) * 24 *
			3600, e = { value: b, expire: d }; return !userData &&
			QQWB.log.error("store can't set value for key " + a +
				", userData is unavaiable, please try later"),
		userData && userData.load(storeName), userData &&
			userData.setAttribute(a, JSON.stringify(e)), userData
			&& userData.save(storeName), userData.getAttribute(a)
		}, get: function(a, b) { a = "k" + a, !userData &&
			QQWB.log.error("store can't get value for key " + a +
				", userData is unavaiable, please try later"),
			userData && userData.load(storeName); var c = userData
			&& userData.getAttribute(a); return c && (c =
				JSON.parse(c)) && c.value &&
			QQWB.time.secondsNow() < c.expire ? c.value : b }, del:
			function(a) { return a = "k" + a, !userData &&
				QQWB.log.error("store can't delete value for
					key " + a + ", userData is unavaiable,
					please try later"), userData &&
					userData.load(storeName), userData &&
			userData.removeAttribute(a), userData &&
			userData.save(storeName), !userData.getAttribute(a) }
		}) } else QQWB.log.warning("T.localStorage object isn't
			initialized, do check before use"); QQWB.localStorage
		&& (QQWB._alias.call(QQWB.localStorage, "save",
					QQWB.localStorage.set),
				QQWB._alias.call(QQWB.localStorage, "remove",
					QQWB.localStorage.del)),
		QQWB.extend("auth.authWindow", { _width:
			QQWB._const.AUTH_WINDOW_WIDTH, _height:
			QQWB._const.AUTH_WINDOW_HEIGHT, _name:
			QQWB._const.AUTH_WINDOW_NAME, _url: QQWB._domain.auth,
		_attribs:
			"toolbar=no,menubar=no,scrollbars=yes,resizable=yes,location=yes,status=no",
		_authorizing: !1, _window: null, show: function() { var a, b,
			c, d; return this._authorizing ? this.focus() : (a =
				(window.screenX || window.screenLeft) +
				((window.outerWidth ||
				  document.documentElement.clientWidth) -
				 this._width) / 2, b = (window.screenY ||
					 window.screenTop) +
				((window.outerHeight ||
				  document.documentElement.clientHeight) -
				 this._height) / 2, c =
				QQWB.queryString.encode({ response_type:
					"token", client_id: QQWB.appkey.value,
				redirect_uri: QQWB._domain.clientproxy, scope:
					"all", status: 0 }), d = ["width=" +
				this._width, "height=" + this._height, "left="
				+ a, "top=" + b], this._window =
				window.open(this._url + "?" + c, this._name, d
					+ "," + this._attribs),
				this._authorizing = !0, function() { var a =
					QQWB.auth.authWindow, b; if
				(a._window.closed) {
					QQWB._token.resolveResponse("error=access_denied"),
					a.close(); return } try { b =
						a._window.location.hash } catch
						(c) { b = null } if (b) { b =
							QQWB.queryString.decode(b.split("#").pop()),
							parseInt(b.status, 10)
								== 200 &&
								QQWB._token.resolveResponse(b),
							a.close(); return }
			setTimeout(arguments.callee, 0) }()), this }, close:
				function() { return this._authorizing = !1,
					this._window ? this._window.closed ?
						this : (this._window.close(),
								this) : this },
					focus: function() { return this._window
						&& this._window.focus(), this }
		}), QQWB.extend("auth", { login: function(a, b) { QQWB._inited
			|| QQWB.log.critical("Library not initialized, call
				T.init() to initialize"); var c =
				QQWB.loginStatus(), d; if (c && a) { a(c);
					return } if (a || b) d = function(c) {
						c.access_token && a ? a(c) :
			c.error && b ? b(c) : QQWB.log.warning("confused result
				of T.login"),
		QQWB.unbind(QQWB.events.USER_LOGGEDIN_EVENT, d),
		QQWB.unbind(QQWB.events.USER_LOGIN_FAILED_EVENT, d), d = null
					},
		QQWB.bind(QQWB.events.USER_LOGGEDIN_EVENT, d),
		QQWB.bind(QQWB.events.USER_LOGIN_FAILED_EVENT, d); return
			QQWB.auth.authWindow.show().focus(), QQWB }, logout:
			function(a) { var b = QQWB.loginStatus(); return
				QQWB.log.info("logging out user..."), b ?
			(QQWB._token.clearAccessToken(),
			 QQWB._token.clearRefreshToken(), QQWB.log.info(b.name
				 + " has logged out successfully")) :
			QQWB.log.warning("oops, user not logged in"), a &&
			a.call(QQWB),
			QQWB.trigger(QQWB.events.USER_LOGGEDOUT_EVENT), QQWB },
		loginStatus: function(a) { var b, c =
			QQWB._token.getAccessToken(), d =
				QQWB._token.getTokenUser(); return c && (b = {
					access_token: c, name: d.name, nick:
					d.nick }), a && a.call(QQWB, b), b }
		}), QQWB._alias("login", QQWB.auth.login),
		QQWB._alias("logout", QQWB.auth.logout),
		QQWB._alias("loginStatus", QQWB.auth.loginStatus),
		QQWB.provide("api", function(a, b, c, d, e) { a =
			this._apiProvider.compat(a), b = b || {}, c = (c ||
				"json").toLowerCase(), d = d || "GET"; var f,
			g, h = c, i = { json: !0, xml: !0 }, j =
			QQWB.deferred.deferred(); h in i || (h = "json"),
		b.oauth_consumer_key = QQWB.appkey.value, b.oauth_token =
			QQWB._token.getAccessToken(), b.oauth_version = "2.0",
		b.format = h, f = j.promise(), e &&
			QQWB.Array.inArray([QQWB._solution.HTML5_SOLUTION,
				QQWB._solution.FLASH_SOLUTION,
				QQWB._solution.SILVER_LIGHT_SOLUTION], e) ?
			(QQWB.log.warning("forced to use solution " + e),
			 QQWB._solution[e] || (QQWB.log.warning("forced to use
					 solution " + e + ", this solution is
					 not inited, initialzing..."),
				 QQWB._solution.initSolution[e]), g =
			 QQWB._solution[e]) : g =
			QQWB.browser.feature.postmessage &&
			QQWB._solution[QQWB._solution.HTML5_SOLUTION] ||
			QQWB.browser.feature.flash &&
			QQWB._solution[QQWB._solution.FLASH_SOLUTION] ||
			QQWB.browser.feature.silverlight &&
			QQWB._solution[QQWB._solution.SILVER_LIGHT_SOLUTION];
		if (!g || g.readyState === 2) return
			QQWB.log.critical("solution error"), j.reject(-1,
					"solution error", 0), f; if
				(g.readyState === 0) return
				QQWB.log.warning("solution is not ready, your
						api call request has been
						cached, will invoke immediately
						when solution is ready"),
				g.promise.done(function() {
					QQWB.log.info('invoking cached api call
						"QQWB.api( ' + [a, b, c,
					d].join(",") + ')"...'), QQWB.api(a, b,
					c, d).success(function() {
						j.resolveWith(j,
							QQWB.Array.fromArguments(arguments))
					}).error(function() { j.rejectWith(j,
							QQWB.Array.fromArguments(arguments))
					}) }).fail(function() {
						QQWB.log.error("can't invoking
							cached api call
							\"QQWB.api( " + [a, b,
						c, d].join(",") + ')"'),
						j.rejectWith(j,
							QQWB.Array.fromArguments(arguments))
					}), f; QQWB.api.id ||
		QQWB.extend(QQWB.api, { id: 0, total: function() { return
			QQWB.api.id } }), QQWB.api.id++, QQWB.log.info("[" +
				QQWB.api.id + '] requesting data "' +
				QQWB._apiProvider.describe(a) + '" from
				server...'); if (g ===
					QQWB._solution[QQWB._solution.HTML5_SOLUTION])
				{ var k = document.getElementById(g.id); if
					(!k) QQWB.log.critical("server proxy
							not found"),
					j.reject(-1, "server proxy not found",
							0); else if (k.src !==
								QQWB._domain.serverproxy)
								QQWB.log.critical("server
										proxy
										is
										not
										valid,
										src
										attribute
										has
										unexpected
										value"),
							j.reject(-1, "server
									proxy
									not
									valid",
									0);
					else { QQWB.api._deferredCollection ||
						QQWB.extend(QQWB.api, {
							_deferredCollection:
						{}, deferredAt: function(a) {
										    if
							(this._deferredCollection[a])
							return
							this._deferredCollection[a];
						QQWB.log.warning("get deferred
							object has failed, that
							object does not exist
							at index " + a) },
							uncollect: function(a)
						{ this._deferredCollection[a] ?
							delete
							this._deferredCollection[a]
							:
							QQWB.log.warning("uncollect
								deferred object
								has failed,
								that object
								does not exist
								at index " + a)
						}, collect: function(a) { if
							(a.promise) return
								this._deferredCollection[this.id]
								= a, this.id;
							QQWB.log.warning("collect
									a
									non-deferred
									object
									is
									illegal")
						} }), QQWB.api.messageHandler
						||
							(QQWB.provide("api.messageHandler",
								      function(a)
								      { if
									      (QQWB._domain.serverproxy.indexOf(a.origin)
									       !==
									       0)
										      QQWB.log.warning("unexpected
											      message
											      arrived
											      from
											      "
											      +
											      a.origin
											      +
											      "
											      with
											      data
											      "
											      +
											      a.data);
									      else
									      {
										      var
											      b
											      =
											      QQWB.JSON.fromString(a.data),
											      c
												      =
												      b.id,
											      d
												      =
												      QQWB.api.deferredAt(c),
											      e
												      =
												      b.data;
										      d
											      ?
											      (e[0]
											       !==
											       200
											       ?
											       d.reject.apply(d,
												       e)
											       :
											       (e[5]
												==
												"xmltext"
												&&
												(e[3]
												 =
												 QQWB.XML.fromString(e[3])),
												d.resolve(e[3],
													e[2],
													e[4])),
											       QQWB.api.uncollect(c))
											      :
											      QQWB.log.warning("related
													      deferred
													      object
													      not
													      found,
													      it
													      shouldn't
													      happen")
									      }
								      }),
							window.addEventListener
								?
								window.addEventListener("message",
										QQWB.api.messageHandler,
										!1)
								:
								window.attachEvent
								&&
								window.attachEvent("onmessage",
										QQWB.api.messageHandler));
						try { var l =
							QQWB.api.collect(j);
							setTimeout(function() {
								k.contentWindow.postMessage(QQWB.JSON.stringify({
									id: l,
									data:
									[a, b,
									c, d]
								}),
									QQWB._domain.serverproxy)
							}, 0) } catch (m) {
								QQWB.log.critical("post
										message
										to
										server
										proxy
										has
										failed,
										"
										+
										m),
									j.reject(-1,
											m,
											0)
							} } } else g ===
		QQWB._solution[QQWB._solution.FLASH_SOLUTION] &&
			QQWB.io._apiFlashAjax(a, b, c, d).complete(function() {
				var a = QQWB.Array.fromArguments(arguments);
				a[0] !== 200 ? j.reject.apply(j, a) :
				j.resolve(a[3], a[2], a[4]) }); return

    function() { var e = QQWB.api.id; f.complete(function() {
	    QQWB.log.info("*[" + e + "] done"), e = null }); if (QQWB.pingback
	    && QQWB.ping) { function h(e, f, h) { QQWB.ping.pingAPI(a,
		    QQWB.queryString.encode(b), c, d, e, f, h, g.name) }
	    f.success(function(a, b) { h(200, "ok", b) }), f.fail(function(a,
				    b, c) { h(a, b, c) }) } }(), f })
/*
			      |xGv00|6d5d6ce9717937b567ca32a7f3d7c7d0 */

