#' This function corrects some punctuation mistakes that appeared in titles in the
#' Alphabetic Index of Titles
#'
#' @param x vector of characteres
#' @param flag Flag to specify if commas should be substituted by space (by default, flag=1 )
#' @return Vector of the same size as x but with different substitutions
#' @export

f_clean_punct <- function(x,flag=1){
  y=gsub("\\."," ",x)
  if (flag==1) y=gsub(","," ",y)
  y=gsub(",$","",y)
  y=gsub("’|`|'|‘|;|\"|”|“","",y)
  y=gsub("-"," ",y)
  # Use capture and backtracking
  y=gsub("([a-z0-9])\\(","\\1 \\(",y)
  y=gsub("\\)([a-z0-9])","\\) \\1",y)

  y=gsub("\\( ","\\(",y)
  y=gsub(" \\)","\\)",y)
  y=gsub("\\( \\)|\\(\\)","",y)
  y=gsub("\\)\\(","\\) \\(",y)
  y=stringr::str_squish(y)

  y=gsub(" , ",", ",y)
  y=gsub(", , |, , , ",", ",y)
  y=gsub("\\(, |\\(,","\\(",y)
  y=gsub(", \\)|,\\)","\\)",y)
  y=gsub("\\( *","\\(",y)
  y=gsub(" *\\)","\\)",y)
  y=gsub("\\( *\\)|\\(\\)","",y)
  y=gsub("\\(([vx0-9]) ","\\1 ",y)
  y=stringr::str_squish(y)
  y
}

#' This function homogenizes some symbols that appeared in the
#' Alphabetic Index of Titles
#'
#' @param x vector of characteres
#' @return Vector of the same size as x but with different substitutions
#' @export

f_change_syms <- function(x){
  # NOTE: For 1940: private and government worker are separated. To be able to compare it with 1930 I assigned them w
  # worker
  ## Parentheses
  y=gsub("\\(oa, pw, or gw\\)|\\( *[0o]a p* w *or g *w *\\)|\\([0o]a, p* w *or g *w *\\)|\\([0o]a, p* w, *or g *w *\\)|\\( *[0o]a p* w *dr g *w *\\)|\\( *[0o] *a p *w g *w *\\)","\\(o w\\)",x)
  y=gsub("\\( *p *w or *g *w\\)|\\( *g *w or *p *w\\)","\\(w\\)",y)
  y=gsub("\\(oa or pw\\)|\\([0o]a or p *w\\)|\\( *[0o]a *or p *w *\\)|\\( *[0o] *a dr p *w *\\)|\\( *[0o] *a p *w *\\)","\\(o w\\)",y)
  y=gsub("\\( *[0o]a *or g *w *\\)|\\( *[0o] *a dr g *w *\\)|\\( *[0o] *a g *w *\\)","\\(o w\\)",y)
  # Employer or own account
  y=gsub("\\( *e *m or [0o] *a *\\)|\\( *e *m *[0o] *a *\\)","\\(e o\\)",y)
  y=gsub("\\( *e *[0o] *\\)|\\( *e *or *[0o] *\\)|\\( *e *[0o] *\\)","\\(e o\\)",y)
  y=gsub("\\( *e *[0o]a *\\)|\\( *e *or *[0o]a *\\)|\\( *e *[0o]a *\\)","\\(e o\\)",y)
  # Employer
  y=gsub("\\( *e *m *\\)|\\( *e *\\)|\\<€\\>","\\(e\\)",y)
  # Own account
  y=gsub("\\( *[0o] *a *\\)|\\( *o *\\)","\\(o\\)",y)
  #government, private worker
  y=gsub("\\( *p *w\\)","\\(w\\)",y)
  y=gsub("\\( *g *w\\)","\\(w\\)",y)
  # worker
  y=gsub("\\( *working out *\\)|\\( *w *\\)","\\(w\\)",y)
  # owner or worker (no such symbol for 1920)
  y=gsub("\\( *[0o] *or *w *\\)|\\( *[0o] *w *\\)","\\(o w\\)",y)
  # non paid worker
  y=gsub("\\( *n *p *\\)|\\( *n *r *\\)","\\(np\\)",y)


  ## No parentheses
  # Employer or own account
  y=gsub("\\<e *m or [0o] *a\\>|\\<e *m *[0o] *a\\>","e o",y)
  y=gsub("\\<e *[0o]\\>|\\<e *or *[0o]\\>|\\<e *[0o]\\>","e o",y)
  y=gsub("\\<e *[0o]a\\>|\\<e *or *[0o]a\\>|\\<e *[0o]a\\>","e o",y)
  #Employer
  y=gsub("\\<e *m\\>|\\<e\\>","e",y)
  #own account
  y=gsub("\\<[0o] *a\\>|\\<o\\>","o",y)
  # worker
  y=gsub("\\<working out\\>|\\<w\\>","w",y)
  # owner or worker (no such symbol for 1920)
  y=gsub("\\<[0o] *or *w\\>|\\<[0o] *w\\>|\\<[0o]a *or *w\\>","o w",y)
  # non paid worker
  y=gsub("\\<n *p\\>|\\<n *r\\>","np",y)

  # NOTE: For 1940: private and government worker are separated. To be able to compare it with 1930 I assigned them w
  # worker
  y=gsub("\\<[0o]a, p* w, *or g *w\\>|\\<[0o]a p* w *or g *w\\>|\\<[0o]a p* w *dr g *w\\>|\\<[0o] *a p *w g *w\\>","o w",y)
  y=gsub("\\<p *w or *g *w\\>|\\<g *w or *p *w\\>","w",y)
  y=gsub("\\<[0o]a *or p *w\\>|\\<[0o] *a dr p *w\\>|\\<[0o] *a p *w\\>","o w",y)
  y=gsub("\\<[0o]a *or g *w\\>|\\<[0o] *a dr g *w\\>|\\<[0o] *a g *w\\>","o w",y)
  y=gsub("\\<p *w\\>","w",y)
  y=gsub("\\<g *w\\>","w",y)


  y=gsub("\\<unless\\>","except",y)

  y=stringr::str_squish(y)
  y=gsub(" , ",", ",y)
  y=gsub(", , |, , , ",", ",y)
  y=gsub("\\(, |\\(,","\\(",y)
  y=gsub(", \\)|,\\)","\\)",y)
  y=gsub("\\( *","\\(",y)
  y=gsub(" *\\)","\\)",y)
  y=gsub("\\( *\\)|\\(\\)","",y)
  y=stringr::str_squish(y)
  y
}



#' This function homogenizes some symbols that appeared in the
#' Alphabetic Index of Titles
#'
#' @param x vector of characters
#' @return Vector of the same size as x but with different substitutions
#' @export

f_change_words <- function(x){
  y=gsub("\\( *e *t *c *\\)|\\( *e *t *e *\\)","\\(etc\\)",x)
  y=gsub("\\( *n *e *c *\\)|\\( *n *s *c *\\)|\\( *n *c *c *\\)","\\(nec\\)",y)
  y=gsub("\\( *a *o *s *\\)|\\( *n *o *s *\\)","\\(nos\\)",y)
  y=gsub("\\( *n *s *\\)|\\( *n *a *\\)|\\( *n *g *\\)|\\( *not specified *\\)|\\( *not .*specified *\\)","\\(ns\\)",y)
  y=gsub("\\(orns\\)","\\(or ns\\)",y)
  y=gsub("\\(not elsewhere classified\\)","\\(nec\\)",y)
  y=gsub("\\( *m *d *\\)","\\(md\\)",y)

  y=gsub("\\<e *t *c\\>|\\<e *t *e\\>","etc",y)
  y=gsub("\\<n *e *c\\>|\\<n *s *c\\>|\\<n *c *c\\>","nec",y)
  y=gsub("\\<a *o *s\\>|\\<n *o *s\\>","nos",y)
  y=gsub("\\<n *s\\>|\\<n *a\\>|\\<n *g\\>|\\<not specified\\>","ns",y)
  y=gsub("\\<orns\\>","or ns",y)
  y=gsub("\\<not elsewhere classified\\>","nec",y)
  y=gsub("\\<m *d\\>","md",y)
  y=gsub("\\<u *s\\>|\\<united states\\>","us",y)
  y=gsub("\\<b *r\\>","or",y)
  y=stringr::str_squish(y)
  y=gsub(" , ",", ",y)
  y=gsub(", , |, , , ",", ",y)
  y=gsub("\\(, |\\(,","\\(",y)
  y=gsub(", \\)|,\\)","\\)",y)
  y=gsub("\\( *","\\(",y)
  y=gsub(" *\\)","\\)",y)
  y=gsub("\\( *\\)|\\(\\)","",y)
  y=stringr::str_squish(y)
  y
}

#' This function erases a set of symbols that appear in titles in the
#' Alphabetic Index of Titles.
#' This function is a companion to f_change_syms.
#'
#' @param x vector of characteres
#' @return Vector of the same size as x but with different substitutions
#' @export

f_clean_syms <- function(x){
  ## Parentheses
  # Employer or own account
  y=gsub("\\( *e *m or [0o] *a *\\)|\\( *e *m *[0o] *a *\\)|\\(e o\\)","",x)
  y=gsub("\\( *e *[0o] *\\)|\\( *e *or *[0o] *\\)|\\( *e *[0o] *\\)|\\(e o\\)","",y)
  y=gsub("\\( *e *[0o]a *\\)|\\( *e *or *[0o]a *\\)|\\( *e *[0o]a *\\)|\\(e o\\)","",y)
  # Employer
  y=gsub("\\( *e *m *\\)|\\( *e *\\)|\\(e\\)","",y)
  # Own account
  y=gsub("\\( *[0o] *a *\\)|\\( *o *\\)|\\(o\\)","",y)
  # worker
  y=gsub("\\( *working out *\\)|\\( *w *\\)|\\(w\\)","",y)
  # owner or worker (no such symbol for 1920)
  y=gsub("\\( *[0o] *or *w *\\)|\\( *[0o] *w *\\)|\\(o w\\)","",y)

  y=gsub("\\( *n *p *\\)|\\( *n *r *\\)|\\(np\\)","",y)

  # NOTE: For 1940: private and government worker are separated. To be able to compare it with 1930 I assigned them w
  # worker
  y=gsub("\\( *[0o]a p* w *or g *w *\\)|\\( *[0o]a p* w *dr g *w *\\)|\\( *[0o] *a p *w g *w *\\)","",y)
  y=gsub("\\( *p *w or *g *w\\)","",y)
  y=gsub("\\( *[0o]a *or p *w *\\)|\\( *[0o] *a dr p *w *\\)|\\( *[0o] *a p *w *\\)","",y)
  y=gsub("\\( *[0o]a *or g *w *\\)|\\( *[0o] *a dr g *w *\\)|\\( *[0o] *a g *w *\\)","",y)
  y=gsub("\\( *p *w\\)","",y)
  y=gsub("\\( *g *w\\)","",y)


  #Eliminate when (any) appears. We could also eliminate when \\<any\\> appears, but that might affect phrases that contain any
  y=gsub("\\( *any *\\)","",y)

  ## No parentheses
  # Employer or own account
  y=gsub("\\<e *m or [0o] *a\\>|\\<e *m *[0o] *a\\>|\\<e o\\>","",x)
  y=gsub("\\<e *[0o]\\>|\\<e *or *[0o]\\>|\\<e *[0o]\\>|\\<e o\\>","",y)
  y=gsub("\\<e *[0o]a\\>|\\<e *or *[0o]a\\>|\\<e *[0o]a\\>|\\<e o\\>","",y)
  #Employer
  y=gsub("\\<e *m\\>|\\<e\\>","",y)
  #own account
  y=gsub("\\<[0o] *a\\>|\\<o\\>","",y)
  # worker
  y=gsub("\\<working out\\>|\\<w\\>","",y)
  # owner or worker (no such symbol for 1920)
  y=gsub("\\<[0o] *or *w\\>|\\<[0o] *w\\>","",y)

  y=gsub("\\<n *p\\>|\\<n *r\\>|\\<np\\>","",y)
  y=stringr::str_squish(y)
  y=stringr::str_squish(gsub("\\( *\\)|\\( *or *\\)","",y))
  y=gsub(" , ",", ",y)
  y=gsub(", , |, , , ",", ",y)
  y=gsub("\\(, |\\(,","\\(",y)
  y=gsub(", \\)|,\\)","\\)",y)
  y=gsub("\\( *","\\(",y)
  y=gsub(" *\\)","\\)",y)
  y=gsub("\\( *\\)|\\(\\)","",y)
  y=stringr::str_squish(y)
}

#' This function erases a set of symbols that appear in titles in the
#' Alphabetic Index of Titles.
#' This function is a companion to f_change_words.
#'
#' @param x vector of characteres
#' @return Vector of the same size as x but with different substitutions
#' @export

f_clean_words <- function(x){
  y=gsub("\\( *e *t *c *\\)|\\( *e *t *e *\\)|\\(etc\\)","",x)
  y=gsub("\\( *n *e *c *\\)|\\( *n *s *c *\\)|\\( *n *c *c *\\)|\\(n e c\\)|\\(nec\\)","",y)
  y=gsub("\\( *a *o *s *\\)|\\( *n *o *s *\\)|\\(n o s\\)|\\(nos\\)","",y)
  y=gsub("\\( *n *s *\\)|\\( *n *a *\\)|\\(n s\\)|\\(ns\\)","",y)
  y=gsub("\\(not elsewhere classified\\)|\\(n e c\\)|\\(nec\\)","",y)
  y=gsub("\\( *m *d *\\)|\\(m d\\)|\\(md\\)","",y)


  y=gsub("\\<e *t *c\\>|\\<e *t *e\\>|\\<etc\\>","",y)
  y=gsub("\\<n *e *c\\>|\\<n *s *c\\>|\\<n *c *c\\>|\\<n e c\\>|\\<nec\\>","",y)
  y=gsub("\\<a *o *s\\>|\\<n *o *s\\>|\\<n o s\\>|\\<nos\\>","",y)
  y=gsub("\\<n *s\\>|\\<n *a\\>|\\<n s\\>|\\<ns\\>","",y)
  y=gsub("\\<not elsewhere classified\\>|\\<n e c\\>|\\<nec\\>","",y)
  y=gsub("\\<m *d\\>","",y)
  #  y=gsub("\\<u *s\\>|\\<united states\\>|\\<us\\>","",y)
  y=stringr::str_squish(y)
  y=gsub(" , ",", ",y)
  y=gsub(", , |, , , ",", ",y)
  y=gsub("\\(, |\\(,","\\(",y)
  y=gsub(", \\)|,\\)","\\)",y)
  y=gsub("\\( *","\\(",y)
  y=gsub(" *\\)","\\)",y)
  y=gsub("\\( *\\)|\\(\\)","",y)
  y=stringr::str_squish(y)
  y
}

#' This function erases some conjunctions, and corrects some symbols that
#' appeared in the Alphabetic Index of Titles.
#'
#' @param x vector of characters
#' @return Vector of the same size as x but with different substitutions
#' @export

f_clean_conj <- function(x){
  # Note: We might/should(?) other conjunctions/pronouns different than "or"
  y=gsub("\\<in\\>|\\<and\\>|\\<of\\>|\\<on\\>|\\<for\\>|\\<to\\>","",x)
  y=gsub(" , ",", ",y)
  y=gsub(", , |, , , ",", ",y)
  y=gsub("\\(, |\\(,","\\(",y)
  y=gsub(", \\)|,\\)","\\)",y)
  y=gsub("\\( *","\\(",y)
  y=gsub(" *\\)","\\)",y)
  y=gsub("\\( *\\)|\\(\\)","",y)
  y=str_squish(y)
  y
}

#' This function corrects the way some industries appeared in the
#' Alphabetic Index of Titles. One way of denoting industries is by
#' enclosing their index in parenthesis (x1). Sometimes a parenthesis
#' is missing or there are extra spaces
#'
#' @param x vector of characters
#' @return Vector of the same size as x but with different substitutions
#' @export

f_adjust_ind_number_1940 <- function(x){
  y=gsub("\\(([vx][0-9])$","\\(\\1\\)",x)
  y=gsub("\\(([vx][vx])$","\\(\\1\\)",y)
  y=gsub("\\(([0-9][xv])$","\\(\\1\\)",y)
  y=gsub("\\(([0-9]+)$","\\(\\1\\)",y)
  y=gsub("\\(([0-9]+)$","\\(\\1\\)",y)
  y=gsub("^ ([vx][0-9])\\)$","\\(\\1\\)",y)
  y=gsub("^ ([vx][vx])\\)$","\\(\\1\\)",y)
  y=gsub("^ ([0-9][xv])\\)$","\\(\\1\\)",y)
  y=gsub("^([vx][0-9])\\)$","\\(\\1\\)",y)
  y=gsub("^([vx][vx])\\)$","\\(\\1\\)",y)
  y=gsub("^([0-9][xv])\\)$","\\(\\1\\)",y)
  y=gsub("^([vx][0-9])$","\\(\\1\\)",y)
  y=gsub("^([vx][vx])$","\\(\\1\\)",y)
  y=gsub("^([0-9][vx])$","\\(\\1\\)",y)
  y=gsub("^([0-9][0-9])$","\\(\\1\\)",y)
  y=gsub(" ([vx][0-9])$","\\(\\1\\)",y)
  y=gsub(" ([vx][vx])$","\\(\\1\\)",y)
  y=gsub(" ([0-9][vx])$"," \\(\\1\\)",y)
  y=gsub(" ([0-9][0-9])$"," \\(\\1\\)",y)
  y=stringr::str_squish(y)
  y
}
